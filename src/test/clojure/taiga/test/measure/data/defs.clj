(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :date "2017-11-15"
      :doc "Common definitions for unit tests." }
    
    taiga.test.measure.data.defs
  
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.measure.data.record :as record])
  
  (:import [java.util Map]
           [clojure.lang IFn$OD]))
;;----------------------------------------------------------------
(defn options 
  (^Map [^IFn$OD median ^long n]
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
  (^Map [^IFn$OD median] (options median (* 32 1024))))
;;----------------------------------------------------------------
(defn model-file [nss options model]
  (let [tokens (s/split nss #"\.")
        folder (apply io/file "tst" (butlast tokens))
        fname (last tokens)
        file (io/file folder 
                      (str fname 
                           "-" (z/count (:data options))
                           "-" (taiga/nterms model) 
                           "-" (:mincount options)
                           "-" (:mtry options)
                           "." (:ext options)))]
    (io/make-parents file) 
    file))
;;----------------------------------------------------------------
(defn serialization-test [nss options model]
  (let [edn-file (model-file nss (assoc options :ext "edn.gz") model)
        ;;pretty-file (model-file nss (assoc options :ext "pretty.edn") model)
        json-file (model-file nss (assoc options :ext "json.gz") model)
        ;;bin-file (model-file nss (assoc options :ext "bin.gz") model)
        _ (io/make-parents edn-file)
        _ (taiga/write-json model json-file)
        _ (taiga/write-edn model edn-file)
        ;;_ (taiga/pprint-model model pretty-file)
        edn-model (taiga/read-edn edn-file)]
    (test/is (= model edn-model))))
;;----------------------------------------------------------------
(defn predictions-file [nss options prefix model]
  (let [tokens (s/split nss #"\.")
        folder (apply io/file "tst" (butlast tokens))
        fname (last tokens)
        file (io/file folder 
                      (str "predictions"
                           "-" prefix 
                           "-" fname  
                           "-" (z/count (:data options))
                           "-" (taiga/nterms model) 
                           "-" (:mincount options)
                           "-" (:mtry options)
                           ".tsv.gz"))]
    (io/make-parents file) 
    file))
;;----------------------------------------------------------------
(defn write-predictions [nss options prefix model predictions]
  (z/write-tsv-file 
    record/tsv-attributes
    predictions 
    (predictions-file nss options prefix model)))
;;----------------------------------------------------------------
;;----------------------------------------------------------------
