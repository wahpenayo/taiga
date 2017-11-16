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
            [taiga.scripts.quantiles.record :as record])
  
  (:import [java.util Map]
           [java.io File]
           [clojure.lang IFn$OD]
           [taiga.ensemble MeanModel]))
;;----------------------------------------------------------------
(def ^:private nss (str *ns*))
(def n (* 1 1 1 8 1024))
(def nterms 2)
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
(defn- write-input-data [prefix data]
  (z/write-tsv-file 
    record/tsv-attributes
    data 
    (input-file prefix (z/count data) "tsv.gz")))
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
    (z/seconds
      "mean" 
      (z/write-tsv-file
        record/tsv-attributes 
        mean 
        (input-file "mean" (z/count mean) "tsv.gz")))
    (z/seconds
      "measure" 
      (z/write-tsv-file
        record/tsv-attributes 
        measure 
        (input-file "measure" (z/count measure) "tsv.gz")))
    (z/seconds
      "test" 
      (z/write-tsv-file
        record/tsv-attributes 
        test 
        (input-file "test" (z/count test) "tsv.gz")))
    {:data mean
     :empirical-distribution-data measure
     :test-data test}))
;;----------------------------------------------------------------
(defn- read-input-data [prefix n]
  (record/read-tsv-file 
    (input-file prefix n "tsv.gz")))
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
        data (read-input-data "mean" n)
        forest (taiga/mean-regression (assoc options :data data))
        edn-file (output-file "mean" options "edn.gz")]
    (io/make-parents edn-file)
    (taiga/write-edn forest edn-file)
    forest))
;;----------------------------------------------------------------
(defn- real-probability-measure-options ^Map [^long n]
  (let [options (mean-regression-options n)
        mean-forest-file (output-file "mean" options "edn.gz")
        _(println mean-forest-file)
        mean-forest (taiga/read-edn mean-forest-file)
        data (read-input-data "measure" n)]
    (assoc options 
           :mean-regression-forest mean-forest
           :empirical-distribution-data data)))
;;----------------------------------------------------------------
(defn real-probability-measure ^MeanModel [^long n]
  (let [options (real-probability-measure-options n)
        #_(pp/pprint (z/clojurize (:mean-regression-forest options)))
        forest (taiga/real-probability-measure options)
        edn-file (output-file "measure" options "edn.gz")]
    (io/make-parents edn-file)
    (taiga/write-edn forest edn-file)
    forest))
;;----------------------------------------------------------------
(defn- write-predictions [prefix options predictions]
  (z/write-tsv-file 
    record/tsv-attributes
    predictions 
    (output-file prefix options "tsv.gz")))
;;----------------------------------------------------------------
