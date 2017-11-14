(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-11-10"
      :date "2017-11-10"
      :doc "Common definitions for benchmark scripts." }
    
    taiga.scripts.quantiles.defs
  
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.scripts.quantiles.record :as record])
  
  (:import [clojure.lang IFn$OD]))
;;----------------------------------------------------------------
(defn options 
  ([^clojure.lang.IFn$OD median ^long n]
    ;; Note: record/generator resets seed each time it's called
  (let [data (z/map (record/generator median) (range (* 3 n))) 
        [train emp test] (z/partition n data)
        _ (test/is (== n (z/count train) (z/count emp) (z/count test)))
        predictors (dissoc record/attributes :ground-truth :prediction)
        m (int (z/count predictors))
        _ (test/is (== 8 (z/count predictors)))
        mtry (Math/min m (int (Math/round (Math/sqrt m))))
        nterms 128
        mincount 127
        options {:data train 
                 :empirical-distribution-data emp
                 :test-data test 
                 :attributes record/attributes
                 :nterms nterms 
                 :mincount mincount 
                 :mtry mtry}]
    _ (test/is (== 10 (count (:attributes options))))
    _ (test/is (== 3 (:mtry options)))
    options))
  ([^clojure.lang.IFn$OD median] (options median (* 32 1024))))
;;----------------------------------------------------------------
(def nss (str *ns*))
(def output-folder 
  (apply io/file "tst" (butlast (s/split nss #"\."))))
(defn output-file [options model prefix suffix]
  (let [file (io/file output-folder 
                      (str prefix
                           "-" (z/count (:data options))
                           "-" (taiga/nterms model) 
                           "-" (:mincount options)
                           "-" (:mtry options)
                           "." suffix))]
    (io/make-parents file) 
    file))
;;----------------------------------------------------------------
(defn write-predictions [nss options prefix model predictions]
  (z/write-tsv-file 
    record/tsv-attributes
    predictions 
    (predictions-file nss options prefix model)))
;;----------------------------------------------------------------
