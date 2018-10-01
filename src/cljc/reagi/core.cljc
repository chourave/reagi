(ns reagi.core
  "Functions and types for functional reactive programming."
  (:refer-clojure :exclude [constantly count cycle deliver filter flatten
                            map mapcat merge reduce remove time])
  #?@(:clj [(:import [clojure.lang IDeref IFn IPending])
            (:require [clojure.core :as core]
                      [clojure.core.async :as a :refer [go go-loop <! >! <!! >!!]])]
      :cljs [(:require [cljs.core :as core]
               [cljs.core.async :as a :refer [<! >!]])
             (:require-macros [reagi.core :refer [behavior]]
               [cljs.core.async.macros :refer [go go-loop]])]))

(defprotocol ^:no-doc Signal
  (complete? [signal]
    "True if the signal's value will no longer change."))

(defn signal?
  "True if the object is a behavior or event stream."
  [x]
  (satisfies? Signal x))

(defprotocol ^:no-doc Boxed
  (unbox [x] "Unbox a boxed value."))

(deftype Completed [x]
  Boxed
  (unbox [_] x))

#?(:clj (ns-unmap *ns* '->Completed))

(defn completed
  "Wraps x to guarantee that it will be the last value in a behavior or event
  stream. The value of x will be cached, and any values after x will be
  ignored."
  [x]
  (Completed. x))

(defn box
  "Box a value to ensure it can be sent through a channel."
  [x]
  (if (instance? Completed x)
    x
    (reify Boxed (unbox [_] x))))

#?(:clj
   (extend-protocol Boxed
     Object (unbox [x] x)
     nil (unbox [x] x)))

#?(:cljs
   (extend-protocol Boxed
     default
     (unbox [x] x)))

(deftype Behavior [func cache]
  IDeref
  (#?(:clj deref :cljs -deref) [behavior]
    (unbox (swap! cache #(if (instance? Completed %) % (func)))))
  Signal
  (complete? [_] (instance? Completed @cache)))

#?(:clj (ns-unmap *ns* '->Behavior))

(defn behavior-call
  "Takes a zero-argument function and yields a Behavior object that will
  evaluate the function each time it is dereferenced. See: behavior."
  [func]
  (Behavior. func (atom nil)))

(defmacro behavior
  "Takes a body of expressions and yields a behavior object that will evaluate
  the body each time it is dereferenced."
  [& form]
  `(behavior-call (fn [] ~@form)))

(defn behavior?
  "Return true if the object is a behavior."
  [x]
  (instance? Behavior x))

(def time
  "A behavior that tracks the current time in seconds."
  #?(
     :clj (behavior (/ (System/nanoTime) 1000000000.0))
     :cljs (behavior (/ (.getTime (js/Date.)) 1000.0))))

(defn delta
  "Return a behavior that tracks the time in seconds from when it was created."
  []
  (let [t @time]
    (behavior (- @time t))))

(defprotocol ^:no-doc Observable
  (port [ob]
    "Return a write-only core.async channel. Any elements send to the port will
    be distributed to the listener channels in parallel. Each listener must
    accept before the next item is distributed.")
  (listen [ob ch] [ob ch context]
    "Add a listener channel to the observable. The channel will be closed
    when the port of the observable is closed. Returns the channel.

    Any events sent to the channel will be boxed to protect the channel from
    nils. To listen for unboxed events, use subscribe."))

(defn- mult*
  "A version of clojure.core.async/mult that fixes ASYNC-64.
  This can be removed once a fix is released for core.async."
  [ch]
  (let [state (atom [true {}])
        m (reify
            a/Mux
            (muxch* [_] ch)
            a/Mult
            (tap* [_ ch close?]
              (let [add-ch    (fn [[o? cs]] [o? (if o? (assoc cs ch close?) cs)])
                    [open? _] (swap! state add-ch)]
                (when-not open? (a/close! ch))
                nil))
            (untap* [_ ch]
              (swap! state (fn [[open? cs]] [open? (dissoc cs ch)]))
              nil)
            (untap-all* [_]
              (swap! state (fn [[open? _]] [open? {}]))))
        dchan (a/chan 1)
        dctr (atom nil)
        done (fn [_] (when (zero? (swap! dctr dec))
                      (a/put! dchan true)))]
    (go-loop []
      (let [val (<! ch)]
        (if (nil? val)
          (let [[_ cs] (swap! state (fn [[_ cs]] [false cs]))]
            (doseq [[c close?] cs]
              (when close? (a/close! c))))
          (let [chs (keys (second @state))]
            (reset! dctr (core/count chs))
            (doseq [c chs]
              (when-not (a/put! c val done)
                (swap! dctr dec)
                (a/untap* m c)))
            (when (seq chs)
              (<! dchan))
            (recur)))))
    m))

#?(:clj
   (defn- peek!! [mult time-ms]
     (let [ch (a/chan)]
       (a/tap mult ch)
       (try
         (if time-ms
           (first (a/alts!! [ch (a/timeout time-ms)]))
           (<!! ch))
         (finally
           (a/untap mult ch))))))

#?(:clj
   (def ^:private dependencies
     (java.util.Collections/synchronizedMap (java.util.WeakHashMap.))))

(defn- depend-on
  "Protect a collection of child objects from being GCed before the parent."
  [parent children]
  #?(:clj (.put dependencies parent children)))

#?(:clj
   (defn- deref-events [mult head ms timeout-val]
     (if-let [hd @head]
       (unbox hd)
       (if-let [val (peek!! mult ms)]
         (unbox val)
         timeout-val))))

#?(:cljs
   (defn- deref-events [head]
     (if-let [hd @head]
       (unbox hd)
       js/undefined)))

(def ^:dynamic *default-context* nil)

(defprotocol ^:no-doc BuildContext
  (setting-up [x])
  (ready [x]))

(defprotocol ^:no-doc Ready
  (ready? [x])
  (when-ready [x f]))

(deftype BuildSyncContext [count]
  BuildContext
  (setting-up [_] (swap! count inc))
  (ready [_] (swap! count dec))

  Ready
  (when-ready [_ f]
    (let [notify (delay (f))]
      (add-watch
       count
       (gensym)
       (fn [key _ _ n]
         (when (zero? n)
           (remove-watch count key)
           @notify))))
    (swap! count identity)))

#?(:clj (ns-unmap *ns* '->BuildSyncContext))

(defmacro after-setup [& body]
  `(when-ready *default-context* (fn [] ~@body)))

(defn build-sync-context []
  (BuildSyncContext. (atom 0)))

(defmacro with-sync-context [& body]
  `(binding [*default-context* (build-sync-context)]
     ~@body))

(extend-type nil
  BuildContext
  (setting-up [_])
  (ready [_]))

(defprotocol ^:no-doc Disposable
  (dispose [x]
    "Clean up any resources an object has before it goes out of scope. In
    Clojure this is called automatically when the object is finalized. In
    ClojureScript this must be called manually.")
  (on-dispose [x f]
    "Add a function to be called when the object is disposed."))

(deftype Events [ch mult head closed disposers]
  IPending
  #?(:clj  (isRealized [_] (not (nil? @head)))
     :cljs (-realized? [_] (not (nil? @head))))

  IDeref
  #?(:clj  (deref [self] (deref-events mult head nil nil))
     :cljs (-deref [self] (deref-events head)))

  #?@(:clj [clojure.lang.IBlockingDeref
            (deref [_ ms timeout-val] (deref-events mult head ms timeout-val))])

  IFn
  #?(:clj  (invoke [stream msg] (do (>!! ch (box msg)) stream))
     :cljs (-invoke [stream msg] (do (go (>! ch (box msg))) stream)))

  Observable
  (port [_] ch)
  (listen [this channel]
    (listen this channel *default-context*))
  (listen [_ channel context]
    (setting-up context)
    (go (if-let [hd @head] (>! channel hd))
        (a/tap mult channel)
        (ready context))
    channel)

  Signal
  (complete? [_] (or @closed (instance? Completed @head)))

  Disposable
  (dispose [_] (doseq [d @disposers] (d)) (a/close! ch))
  (on-dispose [_ d] (swap! disposers conj d))

  #?@(:clj [Object
            (finalize [stream] (dispose stream))]))

#?(:clj (ns-unmap *ns* '->Events))

(def ^:private no-value
  #?(:clj  (Object.)
     :cljs (js/Object.)))

(defn- no-value? [x]
  (identical? x no-value))

(defn events
  "Create a referential stream of events. An initial value may optionally be
  supplied, otherwise the stream will be unrealized until the first value is
  pushed to it. Event streams will deref to the latest value pushed to the
  stream."
  ([] (events no-value))
  ([init]
   (let [in     (a/chan)
         closed (atom false)
         head   (atom (if (no-value? init) nil (box init)))
         out    (a/chan)
         mult   (mult* out)]
     (go (loop [msg init]
           (if (instance? Completed msg)
             (do (a/close! in)
                 (while (<! in)))
             (when-let [msg (<! in)]
               (>! out msg)
               (reset! head msg)
               (recur msg))))
         (a/close! out)
         (reset! closed true))
     (Events. in mult head closed (atom [])))))

(defn events?
  "Return true if the object is a stream of events."
  [x]
  (instance? Events x))

(defn once
  "Create a completed event stream for a single value."
  [value]
  (events (completed value)))

(defn deliver
  "Deliver one or more messages to an event stream."
  {:arglists '([stream & msgs])}
  ([stream])
  ([stream msg]
     (stream msg))
  ([stream msg & msgs]
     (doseq [m (cons msg msgs)]
       (stream m))))

(defn subscribe
  "Deliver events on an event stream to a core.async channel. Returns the
  channel.

  The events from the stream cannot include nil. The channel will be closed when
  the event stream is complete."
  ([stream channel] (subscribe stream channel *default-context*))
  ([stream channel context]
   (let [ch (a/chan 1 (core/map unbox))]
     (a/pipe ch channel)
     (listen stream ch context))
   (depend-on channel [stream])
   channel))

(defn- close-all! [chs]
  (doseq [ch chs]
    (a/close! ch)))

(defn- split-context [streams]
  [(some #(and (satisfies? BuildContext %) %) streams)
   (core/filter #(satisfies? Observable %) streams)])

(defn- listen-all [streams]
  (let [[context streams] (split-context streams)]
    (mapv #(listen % (a/chan) context) streams)))

(defn- connect-port [stream f & args]
  (apply f (concat args [(port stream)])))

(defn merge
  "Combine multiple streams into one. All events from the input streams are
  pushed to the returned stream."
  [& streams]
  (let [chs (listen-all streams)
        [_ streams] (split-context streams)]
    (doto (events)
      (connect-port a/pipe (a/merge chs))
      (on-dispose #(close-all! chs))
      (depend-on streams))))

(defn- zip-ch [ins out]
  (let [index (into {} (map-indexed (fn [i x] [x i]) ins))]
    (go-loop [value (mapv (core/constantly no-value) ins)
              ins   (set ins)]
      (if (seq ins)
        (let [[data in] (a/alts! (vec ins))]
          (if data
            (let [value (assoc value (index in) (unbox data))]
              (when-not (some no-value? value)
                (>! out (box value)))
              (recur value ins))
            (recur value (disj ins in))))
        (a/close! out)))))

(defn zip
  "Combine multiple streams into one. On an event from any input stream, a
  vector will be pushed to the returned stream containing the latest events
  of all input streams."
  [& streams]
  (let [chs (listen-all streams)
        [_ streams] (split-context streams)]
    (doto (events)
      (connect-port zip-ch chs)
      (on-dispose #(close-all! chs))
      (depend-on streams))))

(defn transform
  "Transform a stream through a transducer."
  ([xf stream] (transform xf stream *default-context*))
  ([xf stream context]
   (let [ch (listen stream (a/chan 1 (comp (core/map unbox) xf) context))]
     (doto (events)
       (connect-port a/pipe ch)
       (on-dispose #(a/close! ch))
       (depend-on [stream])))))

(defn- mapcat*
  [f stream context]
  (transform (core/mapcat f) stream context))

(defn- zipmap*
  [map f streams]
  (let [[context] (split-context streams)]
    (map (partial apply f) (apply zip streams) context)))

(defn mapcat
  "Mapcat a function over a stream."
  [f & streams]
  (zipmap* mapcat* f streams))

(defn map*
  ([f stream context]
   (transform (core/map f) stream context)))

(defn map
  "Map a function over a stream."
  [f & streams]
  (zipmap* map* f streams))

(defn filter
  "Filter a stream by a predicate."
  ([pred stream]
   (filter pred stream *default-context*))
  ([pred stream context]
   (transform (core/filter pred) stream context)))

(defn remove
  "Remove all items in a stream the predicate matches."
  ([pred stream]
   (remove pred stream *default-context*))
  ([pred stream context]
   (filter (complement pred) stream context)))

(defn constantly
  "Constantly map the same value over an event stream."
  ([value stream]
   (constantly value stream *default-context*))
  ([value stream context]
   (map (core/constantly value) stream context)))

(defn- reduce-ch [f init in out]
  (go-loop [acc init]
    (if-let [msg (<! in)]
      (let [val (if (no-value? acc)
                  (unbox msg)
                  (f acc (unbox msg)))]
        (>! out (box val))
        (recur val))
      (a/close! out))))

(defn reduce
  "Create a new stream by applying a function to the previous return value and
  the current value of the source stream."
  ([f stream]
   (reduce f no-value stream))
  ([f init stream]
   (reduce f init stream *default-context*))
  ([f init stream context]
   (let [ch (listen stream (a/chan) context)]
     (doto (events init)
       (connect-port reduce-ch f init ch)
       (on-dispose #(a/close! ch))
       (depend-on [stream])))))

(defn count
  "Return an accumulating count of the items in a stream."
  ([stream]
   (count stream *default-context*))
  ([stream context]
   (reduce (fn [x _] (inc x)) 0 stream context)))

(defn accum
  "Change an initial value based on an event stream of functions."
  ([init stream]
   (accum init stream *default-context*))
  ([init stream context]
   (reduce #(%2 %1) init stream context)))

(def ^:private empty-queue
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs cljs.core.PersistentQueue.EMPTY))

(defn buffer
  "Buffer all the events in the stream. A maximum buffer size may be specified,
  in which case the buffer will contain only the last n items. It's recommended
  that a buffer size is specified, otherwise the buffer will grow without limit."
  ([stream]
   (buffer stream *default-context*))
  ([stream-or-n context-or-stream]
   (if (satisfies? Observable context-or-stream)
     (buffer stream-or-n context-or-stream *default-context*)
     (reduce conj empty-queue stream-or-n context-or-stream)))
  ([n stream context]
   {:pre [(integer? n) (pos? n)]}
   (reduce (fn [q x] (conj (if (>= (core/count q) n) (pop q) q) x))
           empty-queue
           stream
           context)))

(defn uniq
  "Remove any successive duplicates from the stream."
  ([stream]
   (uniq stream *default-context*))
  ([stream context]
   (transform (distinct) stream context)))

(defn cycle
  "Incoming events cycle a sequence of values. Useful for switching between
  states."
  ([values stream]
   (cycle values stream *default-context*))
  ([values stream context]
   (->> (reduce (fn [xs _] (next xs)) (core/cycle values) stream context)
        (map first))))

(defn- time-ms []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn- throttle-ch [timeout-ms in out]
  (go-loop [t0 0]
    (if-let [msg (<! in)]
      (let [t1 (time-ms)]
        (if (>= (- t1 t0) timeout-ms)
          (>! out msg))
        (recur t1))
      (a/close! out))))

(defn throttle
  "Remove any events in a stream that occur too soon after the prior event.
  The timeout is specified in milliseconds."
  ([timeout-ms stream]
   (throttle timeout-ms stream *default-context*))
  ([timeout-ms stream context]
   (let [ch (listen stream (a/chan) context)]
     (doto (events)
       (connect-port throttle-ch timeout-ms ch)
       (on-dispose #(a/close! ch))
       (depend-on [stream])))))

(defn- run-sampler [ref interval stop out]
  (go (loop []
        (let [[_ port] (a/alts! [stop (a/timeout interval)])]
          (when (not= port stop)
            (let [val @ref]
              #?(:clj  (>! out (box val))
                 :cljs (when-not (undefined? val) (>! out (box val))))
              (when-not (and (signal? ref) (complete? ref))
                (recur))))))
      (a/close! out)))

(defn sample
  "Turn a reference into an event stream by deref-ing it at fixed intervals.
  The interval time is specified in milliseconds."
  [interval-ms reference]
  (let [stop (a/chan)]
    (doto (events)
      (connect-port run-sampler reference interval-ms stop)
      (on-dispose #(a/close! stop)))))

(defn wait
  "Returns an event stream that will complete unrealized after specified number
  of milliseconds."
  [time-ms]
  (let [stream (events)]
    (go (<! (a/timeout time-ms))
        (a/close! (port stream)))
    stream))

(defn- join-ch [chs out]
  (go (doseq [ch chs]
        (loop []
          (when-let [msg (<! ch)]
            (>! out (box (unbox msg)))
            (recur))))
      (a/close! out)))

(defn join
  "Join several streams together. Events are delivered from the first stream
  until it is completed, then the next stream, until all streams are complete."
  [& streams]
  (let [chs (listen-all streams)
        [_ streams] (split-context streams)]
    (doto (events)
      (connect-port join-ch chs)
      (on-dispose #(close-all! chs))
      (depend-on streams))))

(defn- flatten-ch [in valve out]
  (go (loop [chs {in nil}]
        (if-not (empty? chs)
          (let [[msg port] (a/alts! (conj (vec (keys chs)) valve))]
            (if (identical? port valve)
              (close-all! (keys chs))
              (if msg
                (if (identical? port in)
                  (let [stream (unbox msg)]
                    (recur (assoc chs (listen stream (a/chan)) stream)))
                  (do (>! out (box (unbox msg)))
                      (recur chs)))
                (recur (dissoc chs port)))))))
      (a/close! out)))

(defn flatten
  "Flatten a stream of streams into a stream that contains all the values of
  its components."
  ([stream]
   (flatten stream *default-context*))
  ([stream context]
   (let [ch    (listen stream (a/chan) context)
         valve (a/chan)]
     (doto (events)
       (connect-port flatten-ch ch valve)
       (on-dispose #(a/close! valve))
       (depend-on [stream])))))
