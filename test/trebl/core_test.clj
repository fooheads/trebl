(ns trebl.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [trebl.core :as trebl :refer [example-data]]))


(def example-state (trebl/new-state example-data))
(def example-size (count example-data))
(def example-first-index 0)
(def example-last-index (dec example-size))


(deftest new-state-test
  (is (= {:data {:a 1 :b 2}
          :table [[:a 1] [:b 2]]
          :index 0}
         (trebl/new-state {:a 1 :b 2}))))


(deftest down-test
  (is (= 1 (-> example-state (trebl/down) :index))))


(deftest up-test
  (is (= 3 (-> example-state (trebl/set-index 4) (trebl/up) (trebl/index)))))


(deftest up-from-top-should-be-noop-test
  (is (= example-first-index
         (-> example-state (trebl/index)))))


(deftest down-from-bottom-should-be-noop-test
  (is (= example-last-index
         (-> example-state
             (trebl/set-index example-last-index)
             (trebl/index)))))


(deftest maps-should-be-pushable-test
  (let [state (-> example-state (trebl/set-index 3))]
    (is (true? (trebl/pushable? state)))))


(deftest right-into-map-should-work-test
  (let [state (-> example-state
                  (trebl/set-index 3)
                  (trebl/right))]
    (is (= (:nested-map example-data) (trebl/data state)))
    (is (= 0 (trebl/index state)))))

