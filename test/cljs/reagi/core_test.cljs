(ns reagi.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest testing done)]
                   [cljs.core.async.macros :refer (go)])
  (:require [cemerick.cljs.test :as t]
            [cljs.core.async :refer (<! >! chan timeout)]
            [reagi.core :as r :include-macros true]))

(deftest test-behavior
  (let [a (atom 1)
        b (r/behavior (+ 1 @a))]
    (is (= @b 2))
    (swap! a inc)
    (is (= @b 3))))

(deftest test-behavior
  (is (r/behavior? (r/behavior "foo")))
  (is (not (r/behavior? "foo"))))

(deftest ^:async test-delta
  (let [d (r/delta)]
    (go (<! (timeout 110))
        (is (> @d 0.1))
        (is (< @d 0.2))
        (done))))

(deftest test-boxed
  (is (= (r/unbox 1) 1))
  (is (= (r/unbox nil) nil))
  (is (not= (r/box nil) nil))
  (is (= (r/unbox (r/box nil)) nil))
  (is (= (r/unbox (r/box 1)) 1)))

(deftest ^:async test-event-push
  (let [e (r/events)]
    (go (e 1)
        (<! (timeout 20))
        (is (= 1 @e))
        (e 2)
        (<! (timeout 20))
        (is (= 2 @e))
        (done))))

(deftest ^:async test-event-realized
  (let [e (r/events)]
    (go (is (not (realized? e)))
        (e 1)
        (<! (timeout 20))
        (is (realized? e))
        (done))))

(deftest ^:async test-event-channel
  (let [ch (chan)
        e  (r/events ch)]
    (is (r/closed? e))
    (is (thrown? js/Error (e 1)))
    (go (>! ch :foo)
        (<! (timeout 20))
        (is (realized? e))
        (is (= @e :foo))
        (done))))

(deftest test-events?
  (is (r/events? (r/events)))
  (is (not (r/events? "foo"))))

(defn- push!! [stream & msgs]
  (go (apply r/push! stream msgs)
      (<! (timeout (* 10 (count msgs))))))

(deftest ^:async test-push!
  (let [e (r/events)]
    (go (<! (push!! e 1))
        (is (= 1 @e))
        (<! (push!! e 2 3 4))
        (is (= 4 @e))
        (done))))

(deftest ^:async test-sink!
  (let [e  (r/events)
        ch (chan 1)]
    (r/sink! e ch)
    (go (r/push! e :foo)
        (is (= (<! ch) :foo))
        (done))))

(deftest ^:async test-cons
  (let [e (r/events)
        c (r/cons 5 e)]
    (go (is (realized? c))
        (is (= @c 5))
        (<! (push!! e 10))
        (is (= @c 10))
        (done))))

(deftest ^:async test-zip
  (let [e1 (r/events)
        e2 (r/events)
        z  (r/zip e1 e2)]
    (go (<! (push!! e1 1))
        (<! (push!! e2 2))
        (is (= @z [1 2]))
        (<! (push!! e1 3))
        (is (= @z [3 2]))
        (<! (push!! e2 4))
        (is (= @z [3 4]))
        (done))))

(deftest ^:async test-map-basic
  (let [s (r/events)
        e (r/map inc s)]
    (go (<! (push!! s 1))
        (is (= 2 @e))
        (done))))

(deftest ^:async test-map-multiple
  (let [s1 (r/events)
        s2 (r/events)
        e  (r/map + s1 s2)]
    (go (<! (push!! s1 4))
        (<! (push!! s2 6))
        (is (= @e 10))
        (done))))

(deftest ^:async test-mapcat-basic
  (let [s (r/events)
        e (r/mapcat (comp list inc) s)]
    (go (<! (push!! s 1))
        (is (= 2 @e))
        (done))))

(deftest ^:async test-mapcat-multiple
  (let [s1 (r/events)
        s2 (r/events)
        e  (r/mapcat (comp list +) s1 s2)]
    (go (<! (push!! s1 2))
        (<! (push!! s2 3))
        (is (= @e 5))
        (done))))

(deftest ^:async test-filter
  (let [s (r/events)
        e (r/filter even? s)]
    (go (<! (push!! s 1))
        (is (not (realized? e)))
        (<! (push!! s 2 3))
        (is (= @e 2))
        (done))))

(deftest ^:async test-remove
  (let [s (r/events)
        e (r/remove even? s)]
    (go (<! (push!! s 0))
        (is (not (realized? e)))
        (<! (push!! s 1 2))
        (is (= @e 1))
        (done))))

(deftest ^:async test-reduce-no-init
  (let [s (r/events)
        e (r/reduce + s)]
    (go (is (not (realized? e)))
        (<! (push!! s 1))
        (is (realized? e))
        (is (= @e 1))
        (<! (push!! s 2))
        (is (= @e 3))
        (<! (push!! s 3 4))
        (is (= @e 10))
        (done))))

(deftest ^:async test-reduce-init
  (let [s (r/events)
        e (r/reduce + 0 s)]
    (is (realized? e))
    (is (= @e 0))
    (go (<! (push!! s 1))
        (is (= @e 1))
        (<! (push!! s 2 3))
        (is (= @e 6))
        (done))))

(deftest ^:async test-reduce-init-persists
  (let [s (r/events)
        e (r/map inc (r/reduce + 0 s))]
    (go (<! (timeout 20))
        (is (= @e 1))
        (done))))

(deftest ^:async test-uniq
  (let [s (r/events)
        e (r/reduce + 0 (r/uniq s))]
    (go (<! (push!! s 1 1))
        (is (= 1 @e))
        (<! (push!! s 1 2))
        (is (= 3 @e))
        (done))))

(deftest ^:async test-count
  (let [e (r/events)
        c (r/count e)]
    (go (is (= @c 0))
        (<! (push!! e 1))
        (is (= @c 1))
        (<! (push!! e 2 3))
        (is (= @c 3))
        (done))))

(deftest ^:async test-cycle
  (let [s (r/events)
        e (r/cycle [:on :off] s)]
    (go (<! (timeout 20))
        (is (= :on @e))
        (<! (push!! s 1))
        (is (= :off @e))
        (<! (push!! s 1))
        (is (= :on @e))
        (done))))

(deftest ^:async test-constantly
  (let [s (r/events)
        e (r/constantly 1 s)
        a (r/reduce + 0 e)]
    (go (<! (push!! s 2 4 5))
        (is (= @e 1))
        (is (= @a 3))
        (done))))

(deftest ^:async test-throttle
  (let [s (r/events)
        e (r/throttle 100 s)]
    (go (r/push! s 1 2)
        (<! (timeout 20))
        (is (= @e 1))
        (<! (timeout 101))
        (r/push! s 3)
        (<! (timeout 50))
        (r/push! s 4)
        (is (= @e 3))
        (done))))

(deftest ^:async test-sample
  (let [a (atom 0)
        s (r/sample 100 a)]
    (go (<! (timeout 120))
        (is (= @s 0))
        (swap! a inc)
        (is (= @s 0))
        (<! (timeout 120))
        (is (= @s 1))
        (done))))

(deftest ^:async test-dispose
  (let [a (atom nil)
        s (r/events)
        e (r/map #(reset! a %) s)]
    (go (<! (push!! s 1))
        (is (= @a 1))
        (r/dispose e)
        (<! (push!! s 2))
        (is (= @a 1))
        (done))))
