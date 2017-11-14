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
  ([^clojure.lang.Keyword data-key ^Iterable data]
  (let [predictors (dissoc record/attributes :ground-truth :prediction)
        m (int (z/count predictors))
        _ (test/is (== 8 (z/count predictors)))
        mtry (Math/min m (int (Math/round (Math/sqrt m))))
        nterms 128
        mincount 127
        options {data-key data
                 :attributes record/attributes
                 :nterms nterms 
                 :mincount mincount 
                 :mtry mtry}]
    _ (test/is (== 10 (count (:attributes options))))
    _ (test/is (== 3 (:mtry options)))
    options)))
;;----------------------------------------------------------------
(def n (* 1 1 1 4 1024))
(def nss (str *ns*))

(def input-folder 
  (io/file 
    (apply io/file "tst" (butlast (s/split nss #"\."))) 
    "in"))
(defn input-file [prefix n suffix]
  (let [file (io/file input-folder 
                      (str prefix "-" n "." suffix))]
    (io/make-parents file) 
    file))

(def output-folder 
  (io/file 
    (apply io/file "tst" (butlast (s/split nss #"\.")))
    "out"))
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
(defn write-inputs [prefix data]
  (z/write-tsv-file 
    record/tsv-attributes
    data 
    (input-file prefix (z/count data) "tsv.gz")))
(defn read-inputs [prefix n]
  (record/read-tsv-file 
    (input-file prefix n"tsv.gz")))
;;----------------------------------------------------------------
(defn write-predictions [nss options prefix model predictions]
  (z/write-tsv-file 
    record/tsv-attributes
    predictions 
    (output-file options model prefix "tsv.gz")))
;;----------------------------------------------------------------
