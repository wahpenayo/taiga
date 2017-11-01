(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :date "2017-10-31"
      :doc "Common definitions for unit tests." }
    
    taiga.test.regress.data.defs
  
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.regress.data.record :as record])
  
  (:import [clojure.lang IFn$OD]))
;;----------------------------------------------------------------
(defn options 
  ([^clojure.lang.IFn$OD mean ^long n]
  (let [data (z/map (record/generator mean) (range (* 2 n))) 
        [train test] (z/split-at n data)
        _ (test/is (== n (z/count train) (z/count test)))
        predictors (dissoc record/attributes :ground-truth :prediction)
        m (int (z/count predictors))
        _ (test/is (== 8 (z/count predictors)))
        mtry (Math/min m (int (Math/round (Math/sqrt m))))
        nterms 128
        mincount 127
        options {:data train 
                 :test-data test 
                 :attributes record/attributes
                 :nterms nterms 
                 :mincount mincount 
                 :mtry mtry}]
    _ (test/is (== 10 (count (:attributes options))))
    _ (test/is (== 3 (:mtry options)))
    options))
  ([^clojure.lang.IFn$OD mean] (options mean (* 32 1024))))
;;----------------------------------------------------------------
(defn forest-file [nss options forest]
  (let [tokens (s/split nss #"\.")
        folder (apply io/file "tst" (butlast tokens))
        fname (last tokens)
        file (io/file folder 
                      (str fname 
                           "-" (z/count (:data options))
                           "-" (taiga/nterms forest) 
                           "-" (:mincount options)
                           "-" (:mtry options)
                           "." (:ext options)))]
    (io/make-parents file) 
    file))
;;----------------------------------------------------------------
(defn serialization-test [nss options forest]
  (let [edn-file (forest-file nss (assoc options :ext "edn.gz") forest)
        ;;pretty-file (forest-file nss (assoc options :ext "pretty.edn") forest)
        json-file (forest-file nss (assoc options :ext "json.gz") forest)
        ;;bin-file (forest-file nss (assoc options :ext "bin.gz") forest)
        _ (io/make-parents edn-file)
        _ (taiga/write-json forest json-file)
        _ (taiga/write-edn forest edn-file)
        ;;_ (taiga/pprint-forest forest pretty-file)
        edn-forest (taiga/read-edn edn-file)]
    (test/is (= forest edn-forest))))
;;----------------------------------------------------------------
(defn prediction-file [nss options prefix model]
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
(defn predictions 
  (^Iterable [^clojure.lang.IFn$OOD model predictors data k]
    (z/map #(assoc % k (.invokePrim model predictors %)) data))
  (^Iterable [^clojure.lang.IFn$OD model data k]
    (z/map #(assoc % k (.invokePrim model %)) data)))
;;----------------------------------------------------------------
(defn write-predictions [nss options prefix model predictions]
  (record/write-tsv-file predictions (prediction-file nss options prefix model)))
;;----------------------------------------------------------------
(defn print-residual-summary [^IFn$OD y ^IFn$OD yhat data]
  (let [rmad (z/mean-absolute-difference y yhat data)
        ^IFn$OD residual (fn residual ^double [datum] 
                           (- (.invokePrim y datum) 
                              (.invokePrim yhat datum)))
        residuals (z/map-to-doubles residual data)
        rmean (z/mean residuals)
        n (z/count data)
        rmse (Math/sqrt (/ (z/l2-norm residuals) n))]
    (println rmean rmse rmad)
    [rmean rmse rmad]))
;;;----------------------------------------------------------------
(defn by-nterms-file [nss options model]
  (let [tokens (s/split nss #"\.")
        folder (apply io/file "tst" (butlast tokens))
        fname (last tokens)
        file (io/file folder 
                      (str "by-nterms"
                           "-" fname  
                           "-" (z/count (:data options))
                           "-" (taiga/nterms model) 
                           "-" (:mincount options)
                           "-" (:mtry options)
                           ".tsv.gz"))]
    (io/make-parents file) 
    file))
;;----------------------------------------------------------------
(defn by-nterms [nss options forest predictors]
  (let [nterms (taiga/nterms forest)
        train (:data options)
        test (:test-data options)
        delta 4]
    (with-open [w (z/print-writer (by-nterms-file nss options forest))]
      (.println w 
        (s/join 
          "\t" 
          ["nterms" "trainTest" "mad" "mmad"]))
      (loop [n 1]
        (when (< n nterms)
          (when (zero? (rem n 16)) (println "nterms:" n))
          (let [^clojure.lang.IFn$OOD model (taiga/take-terms n forest)
                ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                            (.invokePrim model predictors datum))]
            (.println w 
              (s/join 
                "\t" 
                [n "train"
                 (z/mean-absolute-difference record/y yhat train)
                 (z/mean-absolute-difference record/mean yhat train)]))
            (.println w 
              (s/join 
                "\t" 
                [n "test"
                 (z/mean-absolute-difference record/y yhat test)
                 (z/mean-absolute-difference record/mean yhat test)])))
          (recur (+ delta n)))))))
;;----------------------------------------------------------------
