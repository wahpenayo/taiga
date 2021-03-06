(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com" 
      :date "2018-04-16"
      :doc "Common definitions for unit tests." }
    
    taiga.test.regress.data.defs
  
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.regress.data.record :as record])
  
  (:import [java.util Map]
           [java.io File]
           [clojure.lang IFn IFn$OD IFn$OOD]))
;;----------------------------------------------------------------
(defn options 

  ([^Map attributes 
    ^Map bindings 
    ^IFn generator 
    ^IFn$OD mean 
    sigma
    n]
  (let [n (int n)
        sigma (double sigma)
        data (z/map (generator mean sigma) (range (* 2 n))) 
        [train test] (z/split-at n data)
        _ (test/is (== n (z/count train) (z/count test)))
        m (int (z/count bindings))
        ;;_ (test/is (== 8 (z/count bindings)))
        mtry (Math/min m (int (Math/round (Math/sqrt m))))
        nterms 128
        mincount 127
        options {:data train 
                 :test-data test 
                 :attributes attributes
                 :nterms nterms 
                 :mincount mincount 
                 :mtry mtry}]
    ;;_ (test/is (== 10 (count (:attributes options))))
    ;;_ (test/is (== 3 (:mtry options)))
    options))

  
  ([^Map attributes 
    ^Map bindings 
    ^IFn generator 
    ^IFn$OD mean
    sigma] 
    (options attributes bindings generator mean sigma (* 32 1024))))
;;----------------------------------------------------------------
(defn forest-file [nss options forest]
  (let [tokens (s/split nss #"\.")
        ^File folder (apply io/file "tst" (butlast tokens))
        _ (.mkdirs folder)
        fname (last tokens)
        file (File/createTempFile
               (str fname 
                    "-" (z/count (:data options))
                    "-" (taiga/nterms forest) 
                    "-" (:mincount options)
                    "-" (:mtry options))
               (str "." (:ext options "edn.gz"))
               (io/file folder))]
    (println (.getPath file))
    file))
;;----------------------------------------------------------------
(defn affine-edn-file [nss]
  (let [tokens (s/split nss #"\.")
        ^File folder (apply io/file "tst" (butlast tokens))
        _ (.mkdirs folder)
        fname (last tokens)
        file (File/createTempFile 
               (str fname "-affine-")
               ".edn" 
               (io/file folder))]
    (println (.getPath file))
    file))
;;----------------------------------------------------------------
(defn linear-edn-file [nss]
  (let [tokens (s/split nss #"\.")
        ^File folder (apply io/file "tst" (butlast tokens))
        _ (.mkdirs folder)
        fname (last tokens)
        file (File/createTempFile 
               (str fname "-linear-")
               ".edn" 
               (io/file folder))]
    (println (.getPath file))
    file))
;;----------------------------------------------------------------
(defn json-test [nss options forest]
  (let [json-file (forest-file nss
                               (assoc options :ext "json.gz") 
                               forest)]
        (taiga/write-json forest json-file)))
;;----------------------------------------------------------------
(defn edn-test [model edn-file]
  (taiga/write-edn model edn-file)
  (test/is (= model (taiga/read-edn edn-file))))
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
  (^Iterable [^IFn$OOD model bindings data k]
    (z/map #(assoc % k (.invokePrim model bindings %)) data))
  (^Iterable [^IFn$OD model data k]
    (z/map #(assoc % k (.invokePrim model %)) data)))
;;----------------------------------------------------------------
(defn write-predictions [nss options prefix model predictions]
  (record/write-tsv-file 
    predictions 
    (prediction-file nss options prefix model)))
;;----------------------------------------------------------------
(defn print-residual-summary
  ([^double p ^IFn$OD y ^IFn$OD yhat data]
    (let [^IFn$OD residual (fn residual ^double [datum] 
                             (- (.invokePrim y datum) 
                                (.invokePrim yhat datum)))
          residuals (z/map-to-doubles residual data)
          n (z/count data)
          results {:rmean (z/mean residuals)
                   :rmse (Math/sqrt (/ (z/l2-norm residuals) n))
                   :rmad (z/mean-absolute-difference y yhat data)
                   :rmqr (z/mean-qr-cost p y yhat data)
                   :rmrq (z/mean-rq-cost p y yhat data)}]
      (pp/pprint results)
      results))
  ([^IFn$OD y ^IFn$OD yhat data]
    (print-residual-summary 0.5 y yhat data)))
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
(defn by-nterms [nss options forest bindings]
  (let [nterms (taiga/nterms forest)
        train (:data options)
        test (:test-data options)
        delta 4]
    (with-open [w (z/print-writer
                    (by-nterms-file nss options forest))]
      (.println w 
        (s/join "\t" ["nterms" "trainTest" "mad" "mmad"]))
      (loop [n 1]
        (when (< n nterms)
          (when (zero? (rem n 16)) (println "nterms:" n))
          (let [^IFn$OOD model (taiga/take-terms n forest)
                ^IFn$OD yhat (fn yhat ^double [datum] 
                               (.invokePrim model 
                                 bindings datum))]
            (.println w 
              (s/join 
                "\t" 
                [n "train"
                 (z/mean-absolute-difference 
                   record/y yhat train)
                 (z/mean-absolute-difference 
                   record/mean yhat train)]))
            (.println w 
              (s/join 
                "\t" 
                [n "test"
                 (z/mean-absolute-difference 
                   record/y yhat test)
                 (z/mean-absolute-difference 
                   record/mean yhat test)])))
          (recur (+ delta n)))))))
;;----------------------------------------------------------------
