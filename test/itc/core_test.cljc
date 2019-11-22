(ns itc.core-test
  (:refer-clojure :exclude [peek #?(:cljs -peek)])
  (:require #?(:clj [clojure.test :refer [is deftest testing]]
               :cljs [cljs.test :refer-macros [is deftest testing run-tests]])
            [itc.core :as itc]))

#?(:cljs (enable-console-print!))

(deftest test-events
  (let [event1 (itc/->event 1)
        event2 (itc/->event '(2, 0, 2))
        event3 (itc/->event 3)]
    (testing "equality"
      (is (= event1 (itc/->event 1)))
      (is (not= event1 event2))
      (is (= event2 (itc/->event '(2, 0, 2)))))

    (testing "is leaf"
      (is (true? (itc/-leaf? event1)))
      (is (false? (itc/-leaf? event2))))

    (testing "lift"
      (is (= 2 (:value (itc/-lift event1 1))))
      (is (= 3 (:value (itc/-lift event2 1)))))

    (testing "drop"
      (is (= 0 (:value (itc/-drop event1 1))))
      (is (= 1 (:value (itc/-drop event2 1)))))

    (testing "min"
      (is (= 1 (itc/-min event1)))
      (is (= 2 (itc/-min event2))))

    (testing "max"
      (is (= 1 (itc/-max event1)))
      (is (= 4 (itc/-max event2))))

    (testing "max depth"
      (is (= 0 (itc/-max-depth event1)))
      (is (= 1 (itc/-max-depth event2))))

    (testing "test normalize"
      (let [expect (itc/->event 3)
            event (itc/->event '(2 1 1))]
        (is (= expect (itc/-normalize event))))

      (let [expect (itc/->event '(4 (0 1 0) 1))
            event (itc/->event '(2 (2 1 0) 3))]
        (is (= expect (itc/-normalize event))))

      (is (= event1 (itc/-normalize event1)))
      (is (= event2 (itc/-normalize event2))))

    (testing "test leq"
      (is (itc/-leq event1 event1))
      (is (itc/-leq event1 event2))
      (is (itc/-leq event1 event3))

      (is (false? (itc/-leq event2 event1)))
      (is (itc/-leq event2 event2))
      (is (false? (itc/-leq event2 event3)))

      (is (false? (itc/-leq event3 event1)))
      (is (false? (itc/-leq event3 event2)))
      (is (itc/-leq event3 event3)))

    (testing "join"
      (is (= event1 (itc/-join event1 event1)))
      (is (= event2 (itc/-join event2 event2)))
      (is (= event3 (itc/-join event3 event3)))

      (let [expect (itc/->event '(3 0 1))]
        (is (= expect (itc/-join event2 event3)))))))

(deftest test-ids
  (let [zero (itc/->id 0)
        one (itc/->id 1)
        zero-zero (itc/->id '(0 0))
        zero-one (itc/->id '(0 1))
        one-zero (itc/->id '(1 0))
        one-one (itc/->id '(1 1))]
    (testing "is leaf"
      (is (itc/-leaf? zero))
      (is (itc/-leaf? one))
      (is (not (itc/-leaf? zero-zero)))
      (is (not (itc/-leaf? zero-one)))
      (is (not (itc/-leaf? one-zero)))
      (is (not (itc/-leaf? one-one))))

    (testing "is zero"
      (is (itc/-zero? zero))
      (is (not (itc/-zero? one)))
      (is (not (itc/-zero? zero-zero)))
      (is (not (itc/-zero? zero-one)))
      (is (not (itc/-zero? one-zero)))
      (is (not (itc/-zero? one-one))))

    (testing "is one"
      (is (not (itc/-one? zero)))
      (is (itc/-one? one))
      (is (not (itc/-one? zero-zero)))
      (is (not (itc/-one? zero-one)))
      (is (not (itc/-one? one-zero)))
      (is (not (itc/-one? one-one))))

    (testing "leaf equals"
      (is (= zero (itc/->id 0)))
      (is (not (= one (itc/->id 0))))
      (is (= one (itc/->id 1)))
      (is (not (= zero (itc/->id 1)))))

    (testing "node equals"
      (is (= zero-zero (itc/->id '(0 0))))
      (is (= zero-one (itc/->id '(0 1))))
      (is (= one-zero (itc/->id '(1 0))))
      (is (= one-one (itc/->id '(1 1)))))

    (testing "normalize"
      (is (= zero (itc/-normalize zero)))
      (is (= one (itc/-normalize one)))
      (is (= zero (itc/-normalize zero-zero)))
      (is (= one (itc/-normalize one-one)))
      (is (= zero-one (itc/-normalize zero-one)))
      (is (= one-zero (itc/-normalize one-zero)))
      (is (= zero (itc/-normalize (itc/->id '(0, (0, 0))))))
      (is (= zero (itc/-normalize (itc/->id '((0, 0) 0)))))
      (is (= one-zero (itc/-normalize (itc/->id '(1 (0, 0))))))
      (is (= zero-one (itc/-normalize (itc/->id '((0, 0) 1)))))
      (is (= one (itc/-normalize (itc/->id '(1, (1, 1))))))
      (is (= one (itc/-normalize (itc/->id '((1, 1) 1)))))
      (is (= zero-one (itc/-normalize (itc/->id '(0, (1, 1))))))
      (is (= one-zero (itc/-normalize (itc/->id '((1, 1) 0))))))

    (testing "split leaf"
      (let [[i1 i2] (itc/-split zero)]
        (is (= zero i1))
        (is (= zero i2)))
      (let [[i1 i2] (itc/-split one)]
        (is (= one-zero i1))
        (is (= zero-one i2))))

    (testing "split zero one"
      (is (= [(itc/->id '(0 (1 0))) (itc/->id '(0 (0 1)))]
             (itc/-split zero-one))))

    (testing "split one zero"
      (is (= [(itc/->id '((1 0) 0)) (itc/->id '((0 1) 0))]
             (itc/-split one-zero))))

    (testing "split one one"
      (is (= [one-zero zero-one]
             (itc/-split one-one))))

    (testing "sum leafs"
      (is (= zero (itc/-sum zero zero)))
      (is (= one (itc/-sum zero one)))
      (is (= one (itc/-sum one zero))))

    (testing "sum non leafs"
      (is (= one (itc/-sum one-zero zero-one)))
      (is (= one (itc/-sum zero-one one-zero)))
      (is (= (itc/->id '(1 (1 0))) (itc/-sum one-zero (itc/->id '(0 (1 0)))))))

    (testing "sum of split"
      (let [[i1 i2] (itc/-split one)]
        (is (= one (itc/-sum i1 i2)))))))

(defn normalized?
  [stamp]
  (and (= (itc/-get-id stamp) (itc/-normalize (itc/-get-id stamp)))
       (= (itc/-get-event stamp) (itc/-normalize (itc/-get-event stamp)))))

(deftest test-stamp
  (let [seed-stamp (itc/->stamp)
        [fork1 fork2] (itc/fork seed-stamp)
        joined (itc/join fork1 fork2)
        stamps [seed-stamp fork1 fork2 joined]]
    (testing "seed stamp is one zero"
      (is (itc/-one? (itc/-get-id seed-stamp)))
      (is (itc/-leaf? (itc/-get-event seed-stamp)))
      (is (zero? (itc/-value (itc/-get-event seed-stamp)))))

    (testing "equals"
      (is (= seed-stamp (itc/->stamp)))
      (is (not (= seed-stamp fork1)))
      (is (= fork1 fork1))
      (is (not (= fork1 fork2))))

    (testing "peek"
      (doseq [stamp stamps]
        (let [peek (itc/peek stamp)]
          (is (= 2 (count peek)))
          (is (= stamp (first peek)))
          (is (itc/-zero? (itc/-get-id (second peek))))
          (is (= (itc/-get-event stamp) (itc/-get-event (second peek))))
          (is (normalized? (first peek)))
          (is (normalized? (second peek))))))

    (testing "fork"
      (doseq [stamp stamps]
        (let [fork (itc/fork stamp)
              ids (itc/-split (itc/-get-id stamp))]
          (is (= 2 (count fork)))
          (is (= (itc/-get-event stamp) (itc/-get-event (first fork))))
          (is (= (itc/-get-event stamp) (itc/-get-event (second fork))))
          (is (= (first ids) (itc/-get-id (first fork))))
          (is (= (second ids) (itc/-get-id (second fork))))
          (is (normalized? (first fork)))
          (is (normalized? (second fork))))))

    (testing "join"
      (is (= seed-stamp
             (itc/join fork1 fork2)))
      (is (= seed-stamp
             (itc/join fork2 fork1)))
      (is (normalized? (itc/join fork1 fork2))))

    (testing "event"
      (doseq [stamp stamps]
        (let [evented (itc/event stamp)]
          (is (itc/-leq stamp evented))
          (is (normalized? evented)))))

    (testing "fork event join"
      (let [fork1 (itc/fork seed-stamp)
            event1 (itc/event (first fork1))
            event2 (itc/event (itc/event (second fork1)))
            fork2 (itc/fork event1)
            event11 (itc/event (first fork2))
            join1 (itc/join (second fork2) event2)
            fork22 (itc/fork join1)
            join2 (itc/join (first fork22) event11)
            event3 (itc/event join2)]
        (is (= (itc/->stamp '((1 0) 2)) event3))))

    (testing "leq"
      (let [s1 (itc/->stamp)
            s2 (itc/->stamp)]
        (is (itc/-leq s1 (itc/event s2)))
        (is (false? (itc/-leq (itc/event s2) s1)))))))

#?(:cljs (run-tests 'itc.core-test))