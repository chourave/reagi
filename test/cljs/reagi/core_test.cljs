(ns reagi.core-test
  (:require-macros [cljs.core.async.macros :refer (go)])
  (:require [cljs.test :as t :refer-macros [async is deftest testing]]
            [cljs.core.async :refer (<! >! chan timeout close!)]
            [reagi.core :as r :include-macros true]))

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
  (async done
    (let [d (r/delta)]
      (go (<! (timeout 110))
          (let [val @d]
            (is (< 0.1 val))
            (is (> 0.2 val))
            (done))))))

(deftest test-boxed
  (is (= 1 (r/unbox 1)))
  (is (= nil (r/unbox nil)))
  (is (not= nil (r/box nil)))
  (is (= nil (r/unbox (r/box nil))))
  (is (= 1 (r/unbox (r/box 1)))))

(deftest test-event-push
  (async done
    (let [e (r/events)]
      (go (e 1)
          (<! (timeout 20))
          (is (= 1 @e))
          (e 2)
          (<! (timeout 20))
          (is (= 2 @e))
          (done)))))

(deftest test-event-unrealized
  (let [e (r/events)]
    (is (not (realized? e)))
    (is (undefined? @e))))

(deftest test-event-realized
  (async done
    (let [e (r/events)]
      (go (e 1)
          (<! (timeout 20))
          (is (realized? e))
          (done)))))

(deftest test-event-initial
  (async done
    (let [e (r/events 1)]
      (is (realized? e))
      (is (= 1 @e))
      (go (e 2)
          (<! (timeout 20))
          (is (= 2 @e))
          (done)))))

(deftest test-event-channel
  (async done
    (let [e (r/events)]
      (go (>! (r/port e) :foo)
          (<! (timeout 20))
          (is (realized? e))
          (is (= :foo @e))
          (done)))))

(deftest test-events?
  (is (r/events? (r/events)))
  (is (not (r/events? "foo"))))

(deftest test-once
  (let [e (r/once :foo)]
    (is (r/complete? e))
    (is (realized? e))
    (is (= :foo @e))))

(defn- deliver! [stream & msgs]
  (go (apply r/deliver stream msgs)
      (<! (timeout (* 20 (count msgs))))))

(deftest test-deliver
  (async done
    (let [e (r/events)]
      (go (<! (deliver! e 1))
          (is (= 1 @e))
          (<! (deliver! e 2 3 4))
          (is (= 4 @e))
          (done)))))

(deftest test-completed-behaviors
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

(deftest test-completed-events
  (async done
    (let [e (r/events)]
      (go (<! (deliver! e 1))
          (is (= 1 @e))
          (<! (deliver! e (r/completed 2)))
          (is (= 2 @e))
          (is (r/complete? e))
          (<! (deliver! e 3))
          (is (= 2 @e))
          (done)))))

(deftest test-completed-initialized
  (async done
    (let [e (r/events (r/completed 1))]
      (is (realized? e))
      (is (= 1 @e))
      (is (r/complete? e))
      (go (<! (deliver! e 2))
          (is (= 1 @e))
          (done)))))

(deftest test-completed-derived
  (async done
    (let [e (r/events)
          m (r/map inc e)]
      (go (<! (deliver! e 1))
          (is (= 2 @m))
          (<! (deliver! e (r/completed 2)))
          (is (= 3 @m))
          (is (r/complete? m))
          (<! (deliver! e 3))
          (is (= 3 @m))
          (done)))))

(deftest test-completed-channel
  (async done
    (let [e (r/events)]
      (go (>! (r/port e) 1)
          (<! (timeout 20))
          (is (= 1 @e))
          (is (not (r/complete? e)))
          (close! (r/port e))
          (<! (timeout 20))
          (is (= 1 @e))
          (is (r/complete? e))
          (done)))))

(deftest test-subscribe
  (async done
    (let [e (r/events)
          ch (chan 1)]
      (r/subscribe e ch)
      (go (r/deliver e :foo)
          (is (= :foo (<! ch)))
          (done)))))

(deftest test-sink-close
  (async done
    (let [e (r/events)
          ch (chan)]
      (go (r/subscribe e ch)
          (<! (timeout 40))
          (close! (r/port e))
          (is (nil? (<! ch)))
          (done)))))

(deftest test-merge
  (async done
    (let [e1 (r/events)
          e2 (r/events)
          m (r/merge e1 e2)]
      (go (<! (deliver! e1 1))
          (is (= 1 @m))
          (<! (deliver! e2 2))
          (is (= 2 @m))
          (done)))))

(deftest test-merge-close
  (async done
    (let [e1 (r/events)
          e2 (r/events)
          m (r/merge e1 e2)]
      (go (>! (r/port e1) 1)
          (<! (timeout 20))
          (is (= 1 @m))
          (close! (r/port e1))
          (>! (r/port e2) 2)
          (<! (timeout 20))
          (is (= 2 @m))
          (done)))))

(deftest test-zip
  (async done
    (let [e1 (r/events)
          e2 (r/events)
          z (r/zip e1 e2)]
      (go (<! (deliver! e1 1))
          (<! (deliver! e2 2))
          (is (= [1 2] @z))
          (<! (deliver! e1 3))
          (is (= [3 2] @z))
          (<! (deliver! e2 4))
          (is (= [3 4] @z))
          (done)))))

(deftest test-zip-close
  (async done
    (let [e1 (r/events)
          e2 (r/events)
          z (r/zip e1 e2)]
      (go (>! (r/port e1) 1)
          (>! (r/port e2) 2)
          (<! (timeout 40))
          (is (= [1 2] @z))
          (close! (r/port e1))
          (>! (r/port e2) 3)
          (<! (timeout 20))
          (is (= [1 3] @z))
          (done)))))

(deftest test-transform
  (async done
    (let [s (r/events)
          e (r/transform (map inc) s)]
      (go (<! (deliver! s 1))
          (is (= 2 @e))
          (done)))))

(deftest test-map-basic
  (async done
    (let [s (r/events)
          e (r/map inc s)]
      (go (<! (deliver! s 1))
          (is (= 2 @e))
          (done)))))

(deftest test-map-multiple
  (async done
    (let [s1 (r/events)
          s2 (r/events)
          e (r/map + s1 s2)]
      (go (<! (deliver! s1 4))
          (<! (deliver! s2 6))
          (is (= @e 10))
          (done)))))

(deftest test-mapcat-basic
  (async done
    (let [s (r/events)
          e (r/mapcat (comp list inc) s)]
      (go (<! (deliver! s 1))
          (is (= 2 @e))
          (done)))))

(deftest test-mapcat-multiple
  (async done
    (let [s1 (r/events)
          s2 (r/events)
          e (r/mapcat (comp list +) s1 s2)]
      (go (<! (deliver! s1 2))
          (<! (deliver! s2 3))
          (is (= 5 @e))
          (done)))))

(deftest test-filter
  (async done
    (let [s (r/events)
          e (r/filter even? s)]
      (go (<! (deliver! s 1))
          (is (not (realized? e)))
          (<! (deliver! s 2 3))
          (is (= 2 @e))
          (done)))))

(deftest test-remove
  (async done
    (let [s (r/events)
          e (r/remove even? s)]
      (go (<! (deliver! s 0))
          (is (not (realized? e)))
          (<! (deliver! s 1 2))
          (is (= 1 @e))
          (done)))))

(deftest test-reduce-no-init
  (async done
    (let [s (r/events)
          e (r/reduce + s)]
      (go (is (not (realized? e)))
          (<! (deliver! s 1))
          (is (realized? e))
          (is (= 1 @e))
          (<! (deliver! s 2))
          (is (= 3 @e))
          (<! (deliver! s 3 4))
          (is (= 10 @e))
          (done)))))

(deftest test-reduce-init
  (async done
    (let [s (r/events)
          e (r/reduce + 0 s)]
      (is (realized? e))
      (is (= 0 @e))
      (go (<! (deliver! s 1))
          (is (= 1 @e))
          (<! (deliver! s 2 3))
          (is (= 6 @e))
          (done)))))

(deftest test-reduce-init-persists
  (async done
    (let [s (r/events)
          e (r/map inc (r/reduce + 0 s))]
      (go (<! (timeout 20))
          (is (= 1 @e))
          (done)))))

(deftest test-buffer-unlimited
  (async done
    (let [s (r/events)
          b (r/buffer s)]
      (is (empty? @b))
      (go (<! (deliver! s 1))
          (is (= [1] @b))
          (<! (deliver! s 2 3 4 5))
          (is (= [1 2 3 4 5] @b))
          (done)))))

(deftest test-buffer-limited
  (async done
    (let [s (r/events)
          b (r/buffer 3 s)]
      (is (empty? @b))
      (go (<! (deliver! s 1))
          (is (= [1] @b))
          (<! (deliver! s 2 3 4 5))
          (is (= [3 4 5] @b))
          (done)))))

(deftest test-buffer-smallest
  (async done
    (let [s (r/events)
          b (r/buffer 1 s)]
      (go (<! (deliver! s 2 3 4 5))
          (is (= [5] @b))
          (done)))))

(deftest test-uniq
  (async done
    (let [s (r/events)
          e (r/reduce + 0 (r/uniq s))]
      (go (<! (deliver! s 1 1))
          (is (= 1 @e))
          (<! (deliver! s 1 2))
          (is (= 3 @e))
          (done)))))

(deftest test-count
  (async done
    (let [e (r/events)
          c (r/count e)]
      (go (is (= 0 @c))
          (<! (deliver! e 1))
          (is (= 1 @c))
          (<! (deliver! e 2 3))
          (is (= 3 @c))
          (done)))))

(deftest test-cycle
  (async done
    (let [s (r/events)
          e (r/cycle [:on :off] s)]
      (go (<! (timeout 20))
          (is (= :on @e))
          (<! (deliver! s 1))
          (is (= :off @e))
          (<! (deliver! s 1))
          (is (= :on @e))
          (done)))))

(deftest test-constantly
  (async done
    (let [s (r/events)
          e (r/constantly 1 s)
          a (r/reduce + 0 e)]
      (go (<! (deliver! s 2 4 5))
          (is (= 1 @e))
          (is (= 3 @a))
          (done)))))

(deftest test-throttle
  (async done
    (let [s (r/events)
          e (r/throttle 100 s)]
      (go (r/deliver s 1 2)
          (<! (timeout 20))
          (is (= 1 @e))
          (<! (timeout 101))
          (r/deliver s 3)
          (<! (timeout 50))
          (r/deliver s 4)
          (is (= 3 @e))
          (done)))))

(deftest test-sample
  (async done
    (let [a (atom 0)
          s (r/sample 100 a)]
      (go (<! (timeout 120))
          (is (= 0 @s))
          (swap! a inc)
          (is (= 0 @s))
          (<! (timeout 120))
          (is (= 1 @s))
          (done)))))

(deftest test-sample-completed
  (async done
    (let [a (atom 0)
          b (r/behavior @a)
          s (r/sample 100 b)]
      (go (<! (timeout 120))
          (is (realized? s))
          (is (not (r/complete? s)))
          (is (= 0 @s))
          (reset! a (r/completed 1))
          (<! (timeout 120))
          (is (r/complete? s))
          (is (= 1 @s))
          (done)))))

(deftest test-dispose
  (async done
    (let [a (atom nil)
          s (r/events)
          e (r/map #(reset! a %) s)]
      (go (<! (deliver! s 1))
          (is (= 1 @a))
          (r/dispose e)
          (<! (deliver! s 2))
          (is (= 1 @a))
          (done)))))

(deftest test-wait
  (async done
    (let [w (r/wait 100)]
      (go (is (not (realized? w)))
          (is (not (r/complete? w)))
          (<! (timeout 110))
          (is (not (realized? w)))
          (is (r/complete? w))
          (done)))))

(deftest test-join
  (async done
    (let [e1 (r/events)
          e2 (r/events)
          j (r/join e1 e2)]
      (go (<! (deliver! e1 1))
          (is (= 1 @j))
          (<! (deliver! e1 (r/completed 2)))
          (is (= 2 @j))
          (<! (deliver! e2 3))
          (is (= 3 @j))
          (done)))))

(deftest test-join-blocking
  (async done
    (let [e1 (r/events)
          e2 (r/events)
          j (r/join e1 e2)
          s (r/reduce + j)]
      (go (<! (deliver! e1 1))
          (<! (deliver! e2 3))
          (is (= 1 @j))
          (is (= 1 @s))
          (<! (deliver! e1 (r/completed 2)))
          (is (= 3 @j))
          (is (= 6 @s))
          (done)))))

(deftest test-join-complete
  (async done
    (let [e1 (r/events)
          e2 (r/events)
          j (r/join e1 e2)]
      (go (<! (deliver! e1 (r/completed 1)))
          (<! (deliver! e2 (r/completed 2)))
          (is (= 2 @j))
          (is (r/complete? j))
          (done)))))

(deftest test-join-once
  (async done
    (let [j (r/join (r/once 1) (r/once 2) (r/once 3))]
      (go (<! (timeout 60))
          (is (realized? j))
          (is (r/complete? j))
          (is (= 3 @j))
          (done)))))

(deftest test-flatten
  (async done
    (let [es (r/events)
          f (r/flatten es)
          e1 (r/events)
          e2 (r/events)]
      (go (<! (deliver! es e1))
          (<! (deliver! e1 1))
          (is (realized? f))
          (is (= 1 @f))
          (<! (deliver! es e2))
          (<! (deliver! e2 2))
          (is (= 2 @f))
          (<! (deliver! e1 3))
          (is (= 3 @f))
          (done)))))

(deftest test-flatten-complete
  (async done
    (let [es (r/events)
          f (r/flatten es)
          e (r/events)]
      (go (<! (deliver! es (r/completed e)))
          (<! (deliver! e 1))
          (is (r/complete? es))
          (is (not (r/complete? e)))
          (is (not (r/complete? f)))
          (<! (deliver! e (r/completed 2)))
          (is (r/complete? e))
          (is (r/complete? f))
          (done)))))
