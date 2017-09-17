(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2017-01-03"
      :doc "Weighted greedy split on categorical attributes." }
    
    taiga.split.numerical.categorical.weighted-scored
  
  (:require [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.tree.split.categorical :as categorical])
  (:import [zana.java.accumulator Accumulator]
           [zana.java.arrays Sorter Arrays]))
;;------------------------------------------------------------------------------
(defn- sorted-xyw [score options]
  (let [data (:data options)
        _(assert (not (empty? data)))
        [kx ^clojure.lang.IFn x] (:this-predictor options)
        _(assert (not (z/singular? x data))
                 (pr-str "trying to split a singular attribute:" kx x "\n" 
                         (z/pprint-str (first data))))
        _(assert (keyword? kx) (print-str "No key for :this-predictor:"
                                          (dissoc options :data)))
        _(assert x (print-str "No value for :this-predictor:"
                              (dissoc options :data)))
        ^clojure.lang.IFn$OD y (:ground-truth options)
        _(assert y (print-str "No :ground-truth:" (dissoc options :data)))
        ^clojure.lang.IFn$OD w (:weight options z/constantly-1d)
        groups (z/group-by-not-nil x data)
        n (z/count groups)
        _ (assert (< 0 n) 
                  (pr-str "no groups for:" x (count data) "\n" 
                          (z/pprint-str (first data))))
        xs (object-array n)
        ys (object-array n)
        ws (object-array n)
        scores (double-array n)
        perm (Sorter/iota n)]
    (z/mapc
      (fn [^long i xsi]
        (let [records (.get groups xsi)
              ^doubles ysi (Arrays/dMap y records)
              ^doubles wsi (Arrays/dMap w records)
              mi (double (score ysi wsi))]
          (aset xs i xsi)
          (aset ys i ysi)
          (aset ws i wsi)
          (aset scores i mi)))
      (range (z/count groups)) 
      (z/sort (z/keys groups)))
    (Sorter/quicksort scores perm)
    (let [xss (Sorter/permute xs perm)
          wss (Sorter/permute ws perm)
          yss (Sorter/permute ys perm)]
      [xss yss wss])))
;;------------------------------------------------------------------------------
(defn split [options]
  (let [^clojure.lang.IFn cost-factory (:cost-factory options)
        ^clojure.lang.IFn feasible? (:feasible? options)
        [^objects xs ^objects ys ^objects ws] (sorted-xyw (:score options) options)
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
                ^doubles wsi (aget ws i)
                n (alength ysi)]
            (assert (== n (alength wsi))) 
            (dotimes [j n] (.add cost1 (aget ysi j) (aget wsi j)))))
        (loop [i (int 0)
               imin (int 0) ;; exclusive upper bound on true xs
               cmin (double (.doubleValue cost1))
               nleft (int 0)]
          (if (< i (- nx 1))
            (let [^doubles ysi (aget ys i)
                  ^doubles wsi (aget ws i)
                  n (alength ysi)]
              (assert (== n (alength wsi))) 
              (dotimes [j n] 
                (let [ysij (aget ysi j)
                      wsij (aget wsi j)]
                  (.delete cost1 ysij wsij)
                  (.add cost0 ysij wsij)))
              (let [cost (double (utils/feasible-cost feasible? cost0 cost1))
                    i+1 (int (inc i))]
                (if (< cost cmin)
                  (recur i+1 i+1 cost (int (.netCount cost0)))
                  (recur i+1 imin cmin nleft))))
            ; done
            (if (or (>= 0 imin) (>= cmin Double/POSITIVE_INFINITY))
              {:cost cmin}
              ;; TODO: max weight branch
              ;; make sure an unseen category goes down the majority vote branch
              (let [cats (if (> nleft (- n nleft))
                           (utils/to-set xs imin nx)
                           (utils/to-set xs 0 imin))]
                (assert (< nleft n))
                {:split (categorical/make kx cats) :cost cmin}))))))))
;;------------------------------------------------------------------------------
