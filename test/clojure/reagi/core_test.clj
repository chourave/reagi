(ns reagi.core-test
  (:require [clojure.test :refer :all]
            [clj-async-test.core :refer :all]
            [reagi.core :as r]
            [clojure.core.async :refer (chan >!! <!! close! pipe thread)]))

(defmethod assert-expr 'lastingly [msg form-with-keyword]
  (let [form (second form-with-keyword)
        args (rest form)
        pred (first form)]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (do
           (Thread/sleep 50)
           (let [values# (list ~@args)
                 result# (apply ~pred values#)]
             (if result#
               (do-report {:type :pass, :message ~msg,
                           :expected '~form, :actual (cons ~pred values#)})
               (do-report {:type :fail, :message ~msg,
                           :expected '~form, :actual (list '~'not (cons '~pred values#))}))
             result#))
         (do
           (do-report {:type :fail, :message ~msg,
                       :expected '~form, :actual (list '~'not (cons '~pred values#))})
           result#)))))

(deftest test-signal?
  (is (r/signal? (r/behavior 1)))
  (is (r/signal? (r/events)))
  (is (not (r/signal? nil)))
  (is (not (r/signal? "foo"))))

(deftest test-behavior
  (let [a (atom 1)
        b (r/behavior (+ 1 @a))]
    (is (= 2 @b))
    (swap! a inc)
    (is (= 3 @b)))
  (is (r/behavior? (r/behavior "foo")))
  (is (not (r/behavior? "foo"))))

(deftest test-delta
  (let [d (r/delta)]
    (Thread/sleep 110)
    (is (< 0.1 @d))
    (is (> 0.5 @d))))

(deftest test-boxed
  (is (= 1 (r/unbox 1)))
  (is (= nil (r/unbox nil)))
  (is (not= nil (r/box nil)))
  (is (= nil (r/unbox (r/box nil))))
  (is (= 1 (r/unbox (r/box 1)))))

(defn- deref! [e]
  (deref e 10000 :timeout))

(deftest test-events
  (testing "push"
    (let [e (r/events)]
      (e 1)
      (is (eventually (= 1 (deref! e))))
      (e 2)
      (is (eventually (= 2 (deref! e))))))
  (testing "realized?"
    (let [e (r/events)]
      (is (not (realized? e)))
      (e 1)
      (is (eventually (realized? e)))))
  (testing "deref"
    (let [e  (r/events)
          t0 (System/currentTimeMillis)]
      (is (= :missing (deref e 100 :missing)))
      (let [t1 (System/currentTimeMillis)]
        (is (<= 100 (- t1 t0) 750)))))
  (testing "initial value"
    (let [e (r/events 1)]
      (is (realized? e))
      (is (= 1 (deref! e)))
      (e 2)
      (is (eventually (= 2 (deref! e))))))
  (testing "channel"
    (let [e (r/events)]
      (>!! (r/port e):foo)
      (is (eventually (realized? e)))
      (is (= :foo (deref! e))))))

(deftest test-events?
  (is (r/events? (r/events)))
  (is (not (r/events? "foo"))))

(deftest test-once
  (let [e (r/once :foo)]
    (is (r/complete? e))
    (is (realized? e))
    (is (= :foo (deref! e)))))

(deftest test-deliver
  (let [e (r/events)]
    (r/deliver e 1)
    (is (eventually (= 1 (deref! e))))
    (r/deliver e 2 3 4)
    (is (eventually (= 4 (deref! e))))))

(deftest test-completed
  (testing "behaviors"
    (let [a (atom nil)
          b (r/behavior @a)]
      (reset! a 1)
      (is (not (r/complete? b)))
      (is (= 1 @b))
      (reset! a (r/completed 2))
      (is (= 2 @b))
      (is (r/complete? b))
      (reset! a 3)
      (is (= 2 @b))
      (reset! a (r/completed 4))
      (is (= 2 @b))))
  (testing "events"
    (let [e (r/events)]
      (r/deliver e 1)
      (is (eventually (= 1 (deref! e))))
      (r/deliver e (r/completed 2))
      (is (eventually (= 2 (deref! e))))
      (is (r/complete? e))
      (r/deliver e 3)
      (is (lastingly (= 2 (deref! e))))))
  (testing "initialized events"
    (let [e (r/events (r/completed 1))]
      (is (realized? e))
      (is (= 1 (deref! e)))
      (is (r/complete? e))
      (r/deliver e 2)
      (is (lastingly (= 1 (deref! e))))))
  (testing "derived events"
    (r/with-sync-context
      (let [e (r/events)
            m (r/map inc e)]
        (r/after-setup
         (r/deliver e 1)
         (is (eventually (= 2 (deref! m))))
         (r/deliver e (r/completed 2))
         (is (eventually (= 3 (deref! m))))
         (is (r/complete? m))
         (r/deliver e 3)
         (is (lastingly (= 3 (deref! m))))))))
  (testing "closed channel"
    (let [e (r/events)]
      (>!! (r/port e) 1)
      (is (= 1 (deref! e)))
      (is (not (r/complete? e)))
      (close! (r/port e))
      (is (= 1 (deref! e)))
      (is (eventually (r/complete? e))))))

(deftest test-subscribe
  (testing "values"
    (let [e  (r/events)
          ch (chan 1)]
      (r/subscribe e ch)
      (r/deliver e :foo)
      (is (= :foo (<!! ch)))))
  (testing "closing"
    (let [e  (r/events)
          ch (chan)]
      (r/subscribe e ch)
      (close! (r/port e))
      (is (nil? (<!! ch))))))

(deftest test-merge
  (testing "merged streams"
    (r/with-sync-context
      (let [e1 (r/events)
            e2 (r/events)
            m  (r/merge e1 e2)]
        (r/after-setup
         (r/deliver e1 1)
         (is (eventually (= 1 (deref! m))))
         (r/deliver e2 2)
         (is (eventually (= 2 (deref! m))))))))
  (testing "closed channels"
    (r/with-sync-context
      (let [e1  (r/events)
            e2  (r/events)
            m   (r/merge e1 e2)]
        (r/after-setup
         (>!! (r/port e1) 1)
         (is (= 1 (deref! m)))
         (close! (r/port e1))
         (>!! (r/port e2) 2)
         (is (eventually (= 2 (deref! m)))))))))

(deftest test-zip
  (testing "zipped streams"
    (r/with-sync-context
      (let [e1 (r/events)
            e2 (r/events)
            z  (r/zip e1 e2)]
        (r/after-setup
         (r/deliver e1 1)
         (r/deliver e2 2)
         (is (eventually (= [1 2] (deref! z))))
         (r/deliver e1 3)
         (is (eventually (= [3 2] (deref! z))))
         (r/deliver e2 4)
         (is (eventually (= [3 4] (deref! z))))))))
  (testing "closed channels"
    (r/with-sync-context
      (let [e1  (r/events)
            e2  (r/events)
            z   (r/zip e1 e2)]
        (r/after-setup
         (>!! (r/port e1) 1)
         (>!! (r/port e2) 2)
         (is (= [1 2] (deref! z)))
         (close! (r/port e1))
         (>!! (r/port e2) 3)
         (is (eventually (= [1 3] (deref! z)))))))))

(deftest test-transform
  (r/with-sync-context
    (let [s (r/events)
          e (r/transform (map inc) s)]
      (r/after-setup
       (r/deliver s 1)
       (is (eventually (= 2 (deref! e))))))))

(deftest test-map
  (testing "Basic operation"
    (r/with-sync-context
      (let [s (r/events)
            e (r/map inc s)]
        (r/after-setup
         (r/deliver s 1)
         (is (eventually (= 2 (deref! e))))))))
  (testing "Multiple streams"
    (r/with-sync-context
      (let [s1 (r/events)
            s2 (r/events)
            e  (r/map + s1 s2)]
        (r/after-setup
         (r/deliver s1 4)
         (r/deliver s2 6)
         (is (eventually (= 10 (deref! e)))))))))

(deftest test-mapcat
  (testing "Basic operation"
    (r/with-sync-context
      (let [s (r/events)
            e (r/mapcat (comp list inc) s)]
        (r/after-setup
         (r/deliver s 1)
         (is (eventually (= 2 (deref! e))))))))
  (testing "Multiple streams"
    (r/with-sync-context
      (let [s1 (r/events)
            s2 (r/events)
            e  (r/mapcat (comp list +) s1 s2)]
        (r/after-setup
         (r/deliver s1 2)
         (r/deliver s2 3)
         (is (eventually (= 5 (deref! e)))))))))

(deftest test-filter
  (r/with-sync-context
    (let [s (r/events)
          e (r/filter even? s)]
      (r/after-setup
       (r/deliver s 1)
       (is (lastingly (not (realized? e))))
       (r/deliver s 2 3)
       (is (eventually (= 2 (deref! e))))))))

(deftest test-remove
  (r/with-sync-context
    (let [s (r/events)
          e (r/remove even? s)]
      (r/after-setup
       (r/deliver s 0)
       (is (lastingly (not (realized? e))))
       (r/deliver s 1 2)
       (is (eventually (= 1 (deref! e))))))))

(deftest test-reduce
  (testing "no initial value"
    (r/with-sync-context
      (let [s (r/events)
            e (r/reduce + s)]
        (r/after-setup
         (is (not (realized? e)))
         (r/deliver s 1)
         (is (eventually (realized? e)))
         (is (= 1 (deref! e)))
         (r/deliver s 2)
         (is (eventually (= 3 (deref! e))))
         (r/deliver s 3 4)
         (is (eventually (= 10 (deref! e))))))))
  (testing "initial value"
    (r/with-sync-context
      (let [s (r/events)
            e (r/reduce + 0 s)]
        (r/after-setup
         (is (realized? e))
         (is (= 0 (deref! e)))
         (r/deliver s 1)
         (is (eventually (= 1 (deref! e))))
         (r/deliver s 2 3)
         (is (eventually (= 6 (deref! e))))))))
  (testing "initial value persists"
    (r/with-sync-context
      (let [s (r/events)
            e (r/map inc (r/reduce + 0 s))]
        (r/after-setup
         (is (= 1 (deref e 1000 :timeout))))))))

(deftest test-buffer
  (testing "unlimited buffer"
    (r/with-sync-context
      (let [s (r/events)
            b (r/buffer s)]
        (r/after-setup
         (is (empty? (deref! b)))
         (r/deliver s 1)
         (is (eventually (= [1] (deref! b))))
         (r/deliver s 2 3 4 5)
         (is (eventually (= [1 2 3 4 5] (deref! b))))))))
  (testing "limited buffer"
    (r/with-sync-context
      (let [s (r/events)
            b (r/buffer 3 s)]
        (r/after-setup
         (is (empty? (deref! b)))
         (r/deliver s 1)
         (is (eventually (= [1] (deref! b))))
         (r/deliver s 2 3 4 5)
         (is (eventually (= [3 4 5] (deref! b))))))))
  (testing "smallest buffer"
    (r/with-sync-context
      (let [s (r/events)
            b (r/buffer 1 s)]
        (r/after-setup
         (r/deliver s 2 3 4 5)
         (is (eventually (= [5] (deref! b))))))))
  (testing "preconditions"
    (is (thrown? AssertionError (r/buffer 0 (r/events))))
    (is (thrown? AssertionError (r/buffer 1.0 (r/events))))))

(deftest test-uniq
  (r/with-sync-context
    (let [s (r/events)
          e (r/reduce + 0 (r/uniq s))]
      (r/after-setup
       (r/deliver s 1 1)
       (is (eventually (= 1 (deref! e))))
       (r/deliver s 1 2)
       (is (eventually (= 3 (deref! e))))))))

(deftest test-count
  (r/with-sync-context
    (let [e (r/events)
          c (r/count e)]
      (r/after-setup
       (is (= 0 (deref! c)))
       (r/deliver e 1)
       (is (eventually (= 1 (deref! c))))
       (r/deliver e 2 3)
       (is (eventually (= 3 (deref! c))))))))

(deftest test-cycle
  (r/with-sync-context
    (let [s (r/events)
          e (r/cycle [:on :off] s)]
      (r/after-setup
       (is (= :on (deref! e)))
       (r/deliver s 1)
       (is (eventually (= :off (deref! e))))
       (r/deliver s 1)
       (is (eventually (= :on (deref! e))))))))

(deftest test-constantly
  (r/with-sync-context
    (let [s (r/events)
          e (r/constantly 1 s)
          a (r/reduce + 0 e)]
      (r/after-setup
       (r/deliver s 2 4 5)
       (is (eventually (= 1 (deref! e))))
       (is (eventually (= 3 (deref! a))))))))

(deftest test-throttle
  (r/with-sync-context
    (let [s (r/events)
          e (r/throttle 500 s)]
      (r/after-setup
       (r/deliver s 1 2)
       (is (eventually (= 1 (deref! e))))
       (is (lastingly (= 1 (deref! e))))
       (Thread/sleep 501)
       (r/deliver s 3)
       (Thread/sleep 100)
       (r/deliver s 4)
       (is (eventually (= 3 (deref! e))))
       (is (lastingly (= 3 (deref! e))))))))

(defn object-collected
  ([reset probe]
   (object-collected reset (constantly nil) probe))
  ([reset trigger probe]
   (reset)
   (System/gc)
   (trigger)
   (Thread/sleep 100)
   (probe)))

(deftest test-gc
  (testing "derived maps"
    (r/with-sync-context
      (let [s (r/events)
            e (r/map inc (r/map inc s))]
        (r/after-setup
         (System/gc)
         (r/deliver s 1)
         (is (eventually (= 3 (deref! e))))))))
  (testing "merge"
    (r/with-sync-context
      (let [s (r/events)
            e (r/merge (r/map inc s))]
        (r/after-setup
         (System/gc)
         (r/deliver s 1)
         (is (eventually (= 2 (deref! e))))))))
  (testing "zip"
    (r/with-sync-context
      (let [s (r/events)
            e (r/zip (r/map inc s) (r/map dec s))]
        (r/after-setup
         (System/gc)
         (r/deliver s 1)
         (is (eventually (= [2 0] (deref! e))))))))
  (testing "flatten"
    (r/with-sync-context
      (let [s (r/events)
            f (r/flatten s)
            a (atom 1)]
        (r/after-setup
         (r/deliver s (r/sample 100 a))
         (r/deliver s (r/once 0))
         (is (eventually(= 1 (deref! f))))
         (System/gc)
         (reset! a 2)
         (is (eventually (= 2 (deref! f))))))))
  (testing "GC unreferenced streams"
    (r/with-sync-context
      (let [a (atom nil)
            s (r/events)]
        (r/after-setup
         (r/map #(reset! a %) s)
         (is (eventually (object-collected
                          #(reset! a nil)
                          #(r/deliver s 1)
                          #(nil? @a)))))))))

(deftest test-sample
  (testing "basic usage"
    (r/with-sync-context
      (let [a (atom 0)
            t0 (System/currentTimeMillis)
            s (r/sample 250 a)]
        (r/after-setup
         (is (= 0 (deref! s)))
         (swap! a inc)
         (is (= 0 (deref! s)))
         (is (eventually (= 1 (deref! s))))
         (let [t1 (System/currentTimeMillis)]
           (is (<= 250 (- t1 t0))))))))
  (testing "completed"
    (r/with-sync-context
      (let [a (atom 0)
            b (r/behavior @a)
            t0 (System/currentTimeMillis)
            s (r/sample 100 b)]
        (r/after-setup
         (is (eventually (realized? s)))
         (is (lastingly (not (r/complete? s))))
         (is (= 0 @s))
         (reset! a (r/completed 1))
         (is (eventually (r/complete? s)))
         (is (eventually (= 1 @s)))
         (let [t1 (System/currentTimeMillis)]
           (is (<= 100 (- t1 t0))))))))
  (testing "thread ends if stream GCed"
    (r/with-sync-context
      (let [a (atom false)]
        (r/sample 100 (r/behavior (reset! a true)))
        (r/after-setup
         (is (eventually (object-collected
                          #(reset! a false)
                          #(= @a false))))))))
  (testing "thread continues if stream not GCed"
    (r/with-sync-context
      (let [a (atom false)
            s (r/sample 100 (r/behavior (reset! a true)))]
        (r/after-setup
         (System/gc)
         (reset! a false)
         (is (eventually (= true @a)))
         s)))))                              ; Prevent s from getting collected

(deftest test-wait
  (let [w (r/wait 100)
        t0 (System/currentTimeMillis)]
    (is (not (realized? w)))
    (is (not (r/complete? w)))
    (is (eventually (r/complete? w)))
    (is (lastingly (not (realized? w))))
    (let [t1 (System/currentTimeMillis)]
      (is (<= 100 (- t1 t0))))))

(deftest test-join
  (testing "basic sequence"
    (r/with-sync-context
      (let [e1 (r/events)
            e2 (r/events)
            j  (r/join e1 e2)]
        (r/after-setup
         (r/deliver e1 1)
         (is (eventually (= 1 (deref! j))))
         (r/deliver e1 (r/completed 2))
         (is (eventually (= 2 (deref! j))))
         (r/deliver e2 3)
         (is (eventually (= 3 (deref! j))))))))
  (testing "blocking"
    (r/with-sync-context
      (let [e1 (r/events)
            e2 (r/events)
            j  (r/join e1 e2)
            s  (r/reduce + j)]
        (r/after-setup
         (r/deliver e1 1)
         (r/deliver e2 3)
         (is (eventually (= 1 (deref! j))))
         (is (eventually (= 1 (deref! s))))
         (r/deliver e1 (r/completed 2))
         (is (eventually (= 3 (deref! j))))
         (is (eventually (= 6 (deref! s))))))))
  (testing "complete"
    (r/with-sync-context
      (let [e1 (r/events)
            e2 (r/events)
            j  (r/join e1 e2)]
        (r/after-setup
         (r/deliver e1 (r/completed 1))
         (r/deliver e2 (r/completed 2))
         (is (eventually (= 2 (deref! j))))
         (is (eventually (r/complete? j)))))))
  (testing "once"
    (r/with-sync-context
      (let [j (r/join (r/once 1) (r/once 2) (r/once 3))]
        (r/after-setup
         (is (eventually (realized? j)))
         (is (eventually (r/complete? j)))
         (is (= 3 (deref! j))))))))

(deftest test-flatten
  (testing "basic operation"
    (r/with-sync-context
      (let [es (r/events)
            f  (r/flatten es)
            e1 (r/events)
            e2 (r/events)]
        (r/after-setup
         (r/deliver es e1)
         (r/deliver e1 1)
         (is (eventually (realized? f)))
         (is (= 1 (deref! f)))
         (r/deliver es e2)
         (r/deliver e2 2)
         (is (eventually (= 2 (deref! f))))
         (r/deliver e1 3)
         (is (eventually (= 3 (deref! f))))))))
  (testing "completion"
    (r/with-sync-context
      (let [es (r/events)
            f  (r/flatten es)
            e  (r/events)]
        (r/after-setup
         (r/deliver es (r/completed e))
         (r/deliver e 1)
         (is (eventually (r/complete? es)))
         (is (not (r/complete? e)))
         (is (not (r/complete? f)))
         (r/deliver e (r/completed 2))
         (is (eventually (r/complete? e)))
         (is (eventually (r/complete? f))))))))
