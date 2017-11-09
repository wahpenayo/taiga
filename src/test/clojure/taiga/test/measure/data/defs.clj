(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :date "2017-11-08"
      :doc "Common definitions for unit tests." }
    
    taiga.test.measure.data.defs
  
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.measure.data.record :as record])
  
  (:import [clojure.lang IFn$OD]))
;;----------------------------------------------------------------
(defn- qcost ^double [^double y ^double q ^double p]
  (let [y-q (- y q)]
    (if (<= 0.0 y-q)
      (* 0.5 (/ y-q (- 1 p)))
      (* 0.5 (/ y-q (- p))))))

(defn quartile-cost [^IFn$OD y ^IFn$OD q25 ^IFn$OD q50 ^IFn$OD q75 
                     ^Iterable data]
  (let [it (z/iterator data)]
    (loop [sum (double 0.0)]
      (if (.hasNext it)
        (let [datum (.next it)
              yi (.invokePrim y datum)
              q25i (.invokePrim q25 datum)
              q50i (.invokePrim q50 datum)
              q75i (.invokePrim q75 datum)]
          (recur (+ sum
                    (qcost yi q25i 0.25)
                    (qcost yi q50i 0.50)
                    (qcost yi q75i 0.75))))
        sum))))
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
#_(defn print-residual-summary [^IFn$OD y ^IFn$OD yhat data]
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
#_(defn by-nterms-file [nss options model]
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
#_(defn by-nterms [nss options model predictors]
   (let [nterms (taiga/nterms model)
         train (:data options)
         test (:test-data options)
         delta 4]
     (with-open [w (z/print-writer (by-nterms-file nss options model))]
       (.println w 
         (s/join 
           "\t" 
           ["nterms" "trainTest" "mad" "mmad"]))
       (loop [n 1]
         (when (< n nterms)
           (when (zero? (rem n 16)) (println "nterms:" n))
           (let [^clojure.lang.IFn$OOD model (taiga/take-terms n model)
                 ^clojure.lang.IFn$OD yhat (fn yhat ^double [datum] 
                                             (.invokePrim model predictors datum))]
             (.println w 
               (s/join 
                 "\t" 
                 [n "train"
                  (z/mean-absolute-difference record/y yhat train)
                  (z/mean-absolute-difference record/q50 yhat train)]))
             (.println w 
               (s/join 
                 "\t" 
                 [n "test"
                  (z/mean-absolute-difference record/y yhat test)
                  (z/mean-absolute-difference record/q50 yhat test)])))
           (recur (+ delta n)))))))
;;----------------------------------------------------------------
