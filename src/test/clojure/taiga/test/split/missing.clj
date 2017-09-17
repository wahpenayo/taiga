(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-30"
      :doc "Check splits with missing attributes." }
    
    taiga.test.split.missing
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.split.api :as split]))
;; mvn -Dtest=taiga.test.split.missing clojure:test > tests.txt
;;------------------------------------------------------------------------------
(defn x ^double [^Double box] (.doubleValue box))
(defn y ^double [^Double box] (if (<= (x box) 0.0) -1.0 1.0))

(test/deftest numerical-right
  (z/reset-mersenne-twister-seeds)
  (let [prng (z/mersenne-twister-generator)
        positive-generator (z/continuous-uniform-generator 0.0 1.0 prng)
        positives (repeatedly 200 positive-generator)
        negative-generator (z/continuous-uniform-generator -1.0 0.0 prng)
        negatives (repeatedly 100 negative-generator)
        data (z/shuffle (z/concat positives negatives) prng)
        n (count data)
        options {:data data
                 :xys (#'split/allocate-cache y nil data)
                 :ground-truth y
                 :this-predictor [:x x]
                 :leaf-learner (z/make-calculator z/mean-accumulator y)
                 :mincount 1
                 :mtry 1
                 :cost-factory z/mssn-accumulator
                 :feasible? (constantly true)
                 :score (z/make-calculator z/mean-accumulator)}
        s (#'split/attribute-split options)
        ^taiga.tree.node.Node split (:split s)
        p (.extract split {:x x})
        datum (Double/valueOf Double/NaN)]
    (test/is (not (p datum)))))
;;------------------------------------------------------------------------------
(test/deftest numerical-left
  (z/reset-mersenne-twister-seeds)
  (let [prng (z/mersenne-twister-generator)
        positive-generator (z/continuous-uniform-generator 0.0 1.0 prng)
        positives (repeatedly 100 positive-generator)
        negative-generator (z/continuous-uniform-generator -1.0 0.0 prng)
        negatives (repeatedly 200 negative-generator)
        data (z/shuffle (z/concat positives negatives) prng)
        n (count data)
        options {:data data
                 :xys (#'split/allocate-cache y nil data)
                 :ground-truth y
                 :this-predictor [:x x]
                 :leaf-learner (z/make-calculator z/mean-accumulator y)
                 :mincount 1
                 :mtry 1
                 :cost-factory z/mssn-accumulator
                 :feasible? (constantly true)
                 :score (z/make-calculator z/mean-accumulator)}
        s (#'split/attribute-split options)
        ^taiga.tree.node.Node split (:split s)
        p (.extract split {:x x})
        datum (Double/valueOf Double/NaN)]
    (test/is (p datum))))
;;------------------------------------------------------------------------------
(test/deftest categorical
  (z/reset-mersenne-twister-seeds)
  (let [prng (z/mersenne-twister-generator)
        a (repeat 200 :a)
        b (repeat 100 :b)
        c (repeat 100 :c)
        d (repeat 100 :d)
        x identity
        y (fn ^double [datum] (if (= :a datum) 0.0 1.0)) 
        data (z/shuffle (z/concat a b c d) prng)
        n (count data)
        x0 (double-array n)
        y0 (double-array n)
        w0 (double-array n)
        y1 (double-array n)
        w1 (double-array n)
        perm (int-array n)
        options {:data data
                 :x0 x0 :y0 y0 :w0 w0 :perm perm :y1 y1 :w1 w1
                 :ground-truth y
                 :this-predictor [:x x]
                 :leaf-learner (z/make-calculator z/positive-fraction-accumulator y)
                 :mincount 1
                 :mtry 1
                 :cost-factory z/gini-accumulator
                 :feasible? (constantly true)
                 :score (z/make-calculator z/positive-fraction-accumulator)}
        s (#'split/attribute-split options)
        ^taiga.tree.node.Node split (:split s)
        p (.extract split {:x identity})]
    ;; missing
    (test/is (not (p nil)))
    ;; previously unseen category
    (test/is (not (p :e)))))
;;------------------------------------------------------------------------------