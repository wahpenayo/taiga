(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2017-01-03"
      :doc "Greedy split on categorical attributes." }
    
    taiga.split.numerical.categorical.scored
  
  (:require [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.tree.split.categorical :as categorical])
  (:import [zana.java.accumulator Accumulator]
           [zana.java.arrays Sorter Arrays]))
;;------------------------------------------------------------------------------
(defn- sorted-xy [score options]
  (let [data (:data options)
        _(assert (not (empty? data)))
        [kx ^clojure.lang.IFn x] (:this-predictor options)
;        _(assert (not (z/singular? x data))
;                 (pr-str "trying to split a singular attribute:" kx x "\n" 
;                         (z/pprint-str (first data))))
        _(assert (keyword? kx) (print-str "No key for :this-predictor:"
                                          (dissoc options :data)))
        _(assert x (print-str "No value for :this-predictor:"
                              (dissoc options :data)))
        ^clojure.lang.IFn$OD y (:ground-truth options)
        _(assert y (print-str "No :ground-truth:" (dissoc options :data)))
        ^java.util.Map groups (z/group-by-not-nil x data)
        n (z/count groups)
        _ (assert (< 0 n) 
                  (pr-str "no groups for:" x (count data) "\n" 
                          (z/pprint-str (first data))))
        xs (object-array n)
        ys (object-array n)
        scores (double-array n)
        perm (Sorter/iota n)]
    (z/mapc
      (fn [^long i xsi]
        (let [records (.get groups xsi)
              ^doubles ysi (Arrays/dMap y records)
              mi (double (score ysi))]
          (aset xs i xsi)
          (aset ys i ysi)
          (aset scores i mi)))
      (range (z/count groups)) 
      ;; TODO: get this from a cached range of the attribute
      (z/sort (z/keys groups)))
    (Sorter/quicksort scores perm)
    (let [xss (Sorter/permute xs perm)
          yss (Sorter/permute ys perm)]
      [xss yss])))
;;------------------------------------------------------------------------------
(defn split [options]
  (let [^clojure.lang.IFn cost-factory (:cost-factory options)
        ^clojure.lang.IFn feasible? (:feasible? options)
        [^objects xs ^objects ys] (sorted-xy (:score options) options)
        [^clojure.lang.Keyword kx ^clojure.lang.IFn x] (:this-predictor options)
        nx (alength xs)
        n (int (utils/sum-alengths ys))]
    (assert (== nx (alength ys)))
    (if (== 1 nx)
      {:cost Double/POSITIVE_INFINITY}
      (let [^Accumulator cost0 (cost-factory)
            ^Accumulator cost1 (cost-factory)]
        (dotimes [i nx] 
          (let [^doubles ysi (aget ys i)
                n (alength ysi)]
            (dotimes [j n] (.add cost1 (aget ysi j)))))
        (loop [i (int 0)
               imin (int 0) ;; exclusive upper bound on true xs
               cmin (double (.doubleValue cost1))
               nleft (int 0)]
          (if (< i (- nx 1))
            (let [^doubles ysi (aget ys i)
                  n (alength ysi)]
              (dotimes [j n] 
                (let [ysij (aget ysi j)]
                  (.delete cost1 ysij)
                  (.add cost0 ysij)))
              (let [cost (double (utils/feasible-cost feasible? cost0 cost1))
                    i+1 (int (inc i))]
                (if (< cost cmin)
                  (recur i+1 i+1 cost (int (.netCount cost0)))
                  (recur i+1 imin cmin nleft))))
            ; done
            (do
              (if (or (>= 0 imin) (>= cmin Double/POSITIVE_INFINITY))
                {:cost cmin}
                ;; TODO: max weight branch
                ;; make sure an unseen category goes down the majority vote branch
                (let [cats (if (> nleft (- n nleft))
                             (utils/to-set xs imin nx)
                             (utils/to-set xs 0 imin))]
                  
                  (assert (< nleft n))
                  {:split (categorical/make kx cats) :cost cmin})))))))))
;;------------------------------------------------------------------------------
