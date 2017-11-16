(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-15"
      :doc "Common definitions for benchmark scripts." }
    
    taiga.scripts.quantiles.defs
  
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.scripts.quantiles.deciles :as deciles]
            [taiga.scripts.quantiles.record :as record])
  
  (:import [java.util Map]
           [java.io File]
           [clojure.lang IFn IFn$OD]
           [taiga.ensemble MeanModel]))
;;----------------------------------------------------------------
(def ^:private nss (str *ns*))
(def n (* 1 1 8 8 1024))
(def nterms 128)
;;----------------------------------------------------------------
(def ^:private input-folder 
  (io/file 
    (apply io/file "tst" (butlast (s/split nss #"\."))) 
    "in"))

(defn- input-file [prefix n suffix]
  (let [file (io/file input-folder 
                      (str prefix "-" n "." suffix))]
    (io/make-parents file) 
    file))
;;----------------------------------------------------------------
(defn generate-data [^long n]
  (let [median (record/make-pyramid-function 16.0)
        data (z/seconds 
               "generate"
               (z/map (record/generator median) 
                      (range (* 3 n)))) 
        [mean measure test] (z/seconds 
                              "partition" 
                              (z/partition n data))]
    (z/write-tsv-file
      record/tsv-attributes 
      mean 
      (input-file "mean" (z/count mean) "tsv.gz"))
    (z/write-tsv-file
      record/tsv-attributes 
      measure 
      (input-file "measure" (z/count measure) "tsv.gz"))
    (z/write-tsv-file
      record/tsv-attributes 
      test 
      (input-file "test" (z/count test) "tsv.gz"))
    {:data mean
     :empirical-distribution-data measure
     :test-data test}))
;;----------------------------------------------------------------
(def ^:private output-folder 
  (io/file 
    (apply io/file "tst" (butlast (s/split nss #"\.")))
    "out"))

(defn- output-file [prefix options suffix]
  (let [file (io/file output-folder 
                      (str prefix
                           "-" (:n options (z/count (:data options)))
                           "-" (:nterms options) 
                           "-" (:mincount options)
                           "-" (:mtry options)
                           "." suffix))]
    (io/make-parents file) 
    file))
;;----------------------------------------------------------------
(defn- mean-regression-options ^Map [^long n]
  (let [predictors (dissoc record/attributes 
                           :ground-truth :prediction)
        m (int (z/count predictors))
        mtry (Math/min m (int (Math/round (Math/sqrt m))))
        mincount 127
        options (taiga/mean-regression-options
                  {:n n
                   :attributes record/attributes
                   :nterms nterms 
                   :mincount mincount 
                   :mtry mtry})]
    options))
;;----------------------------------------------------------------
(defn mean-regression ^MeanModel [^long n]
  (let [options (mean-regression-options n)
        data (record/read-tsv-file 
               (input-file "mean" n "tsv.gz"))
        mean-forest (taiga/mean-regression (assoc options :data data))
        mean-forest-file (output-file "mean" options "edn.gz")]
    (io/make-parents mean-forest-file)
    (taiga/write-edn mean-forest mean-forest-file)
    mean-forest))
;;----------------------------------------------------------------
(defn- real-probability-measure-options ^Map [^long n]
  (let [options (mean-regression-options n)
        mean-forest-file (output-file "mean" options "edn.gz")
        mean-forest (taiga/read-edn mean-forest-file)
        data (record/read-tsv-file 
               (input-file "measure" n "tsv.gz"))]
    (assoc options 
           :mean-regression-forest mean-forest
           :empirical-distribution-data data)))
;;----------------------------------------------------------------
(defn real-probability-measure ^MeanModel [^long n]
  (let [options (real-probability-measure-options n)
        #_(pp/pprint (z/clojurize (:mean-regression-forest options)))
        measure-forest (taiga/real-probability-measure options)
        measure-forest-file (output-file 
                              "measure" options "edn.gz")]
    (io/make-parents measure-forest-file)
    (taiga/write-edn measure-forest measure-forest-file)
    measure-forest))
;;----------------------------------------------------------------
(defn predict [^long n ^String prefix]
  (let [options (real-probability-measure-options n)
        measure-forest-file (output-file 
                              "measure" options "edn.gz")
        measure-forest (taiga/read-edn measure-forest-file)
        predictors (dissoc record/attributes 
                           :ground-truth :prediction)
        predict1 (fn predict1 [datum]
                   (assoc 
                     datum 
                     :qhat (deciles/make 
                             z/quantile 
                             (measure-forest predictors datum))))
        predicted (z/pmap 
                     runningpredict1 
                    (record/read-tsv-file 
                      (input-file prefix n "tsv.gz")))
        predicted-file (output-file prefix options "tsv.gz")]
    (record/write-tsv-file predicted predicted-file)
    predicted))
;;----------------------------------------------------------------
(defn- decile-cost ^double [^IFn$OD y ^IFn deciles ^Iterable data]
  (let [it (z/iterator data)]
    (loop [sum (double 0.0)]
      (if (.hasNext it)
        (let [datum (.next it)
              yi (.invokePrim y datum)
              qi (deciles datum)]
          (recur (+ sum (deciles/cost yi qi))))
        sum))))
;;----------------------------------------------------------------
(defn relative-cost [^long n ^String prefix]
  (let [options (real-probability-measure-options n)
        predicted-file (output-file prefix options "tsv.gz")
        predicted (record/read-tsv-file predicted-file)
        true-cost (decile-cost record/y record/ymu predicted)
        pred-cost (decile-cost record/y record/qhat predicted)]
    (println n prefix (float true-cost) (float pred-cost) 
             (float (/ pred-cost true-cost)))))
;;----------------------------------------------------------------
