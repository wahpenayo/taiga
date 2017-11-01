(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-09"
      :doc "Common definitions for unit tests." }
    
    taiga.test.classify.data.defs
  
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.classify.data.record :as record]))
;;------------------------------------------------------------------------------
(defn weight ^clojure.lang.IFn$OD [^double bias]
  (let [w0 1.0
        w1 (/ (- 1.0 bias) bias)]
    (fn weight ^double [datum] (if (== 0.0 (record/true-class datum)) w0 w1))))
;;------------------------------------------------------------------------------
(defn options 
  ([^clojure.lang.IFn$OD prob ^long n]
  (let [data (z/map (record/generator prob) (range (* 2 n))) 
        [train test] (z/split-at n data)
        _ (test/is (== n (z/count train) (z/count test)))
        predictors (dissoc record/attributes :ground-truth :prediction)
        m (int (z/count predictors))
        _ (test/is (== 8 (z/count predictors)))
        mtry (Math/min m (int (Math/round (Math/sqrt m))))
        nterms 128
        mincount 127
        options {:data train :test-data test :attributes record/attributes
                 :nterms nterms :mincount mincount :mtry mtry}]
    _ (test/is (== 10 (count (:attributes options))))
    _ (test/is (== 3 (:mtry options)))
    options))
  ([^clojure.lang.IFn$OD prob] (options prob (* 32 1024))))
;;------------------------------------------------------------------------------
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
;;------------------------------------------------------------------------------
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
;;------------------------------------------------------------------------------
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
;;------------------------------------------------------------------------------
(defn predictions 
  (^Iterable [^clojure.lang.IFn$OOD model predictors data k]
    (z/map #(assoc % k (.invokePrim model predictors %)) data))
  (^Iterable [^clojure.lang.IFn$OD model data k]
    (z/map #(assoc % k (.invokePrim model %)) data)))
;;------------------------------------------------------------------------------
(defn write-predictions [nss options prefix model predictions]
  (record/write-tsv-file predictions (prediction-file nss options prefix model)))
;;------------------------------------------------------------------------------
(defn print-confusion [y yhat data]
  (let [confusion (z/tabulate y yhat data)
        true-negatives (z/count (z/table-get confusion 0.0 0.0))
        false-negatives (z/count (z/table-get confusion 1.0 0.0))
        false-positives (z/count (z/table-get confusion 0.0 1.0))
        true-positives (z/count (z/table-get confusion 1.0 1.0))]
    (println true-negatives false-negatives)
    (println false-positives true-positives)
    [true-negatives false-negatives false-positives true-positives]))
;;------------------------------------------------------------------------------
(defn confusion [^clojure.lang.IFn$OD c
                 ^clojure.lang.IFn$OD chat 
                 ^Iterable data]
  (let [it (z/iterator data)]
    (loop [true-negatives 0
           false-negatives 0
           false-positives 0
           true-positives 0]
      (if (.hasNext it)
        (let [datum (.next it)
              cd (long (Math/round (.invokePrim c datum)))
              chatd (long (Math/round (.invokePrim chat datum)))]
          (cond (and (== 0 cd) (== 0 chatd))
                (recur (inc true-negatives) false-negatives 
                       false-positives true-positives)
                (and (== 1 cd) (== 0 chatd))
                (recur true-negatives (inc false-negatives) 
                       false-positives true-positives)
                (and (== 0 cd) (== 1 chatd))
                (recur true-negatives false-negatives 
                       (inc false-positives) true-positives)
                (and (== 1 cd) (== 1 chatd))
                (recur true-negatives false-negatives 
                       false-positives (inc true-positives))
                :else
                (throw (IllegalArgumentException. 
                         (pr-str "non-binary class values:" cd chatd)))))
        [true-negatives false-negatives false-positives true-positives]))))
;;------------------------------------------------------------------------------
(defn confusion-rate ^double [[^long true-negatives ^long false-negatives 
                               ^long false-positives ^long true-positives]]
  (/ (double (+ false-negatives false-positives))
     (double (+ true-negatives false-negatives 
                false-positives true-positives))))
;;------------------------------------------------------------------------------
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
;;------------------------------------------------------------------------------
(defn by-nterms [nss options forest predictors]
  (let [nterms (taiga/nterms forest)
        ^clojure.lang.IFn$OD p record/true-probability
        ^clojure.lang.IFn$OD c record/true-class
        train (:data options)
        test (:test-data options)
        delta 4]
    (with-open [w (z/print-writer (by-nterms-file nss options forest))]
      (.println w 
        (s/join 
          "\t" 
          ["nterms" "trainTest" "mad" 
           "trueNegatives" "falseNegatives" "falsePositives" "truePositives"]))
      (loop [n 1]
        (when (< n nterms)
          (when (zero? (rem n 16)) (println "nterms:" n))
          (let [^clojure.lang.IFn$OOD model (taiga/take-terms n forest)
                ^clojure.lang.IFn$OD phat (fn phat ^double [datum] 
                                            (.invokePrim model predictors datum))
                ^clojure.lang.IFn$OD chat (fn chat ^double [datum] 
                                            (if (< (.invokePrim phat datum) 0.5) 
                                              0.0 
                                              1.0))]
            (.println w 
              (s/join 
                "\t" 
                (concat [n "train" (z/mean-absolute-difference p phat train)]
                        (confusion c chat train))))
            (.println w 
              (s/join 
                "\t" 
                (concat [n "test" (z/mean-absolute-difference p phat test)]
                        (confusion c chat test)))))
          (recur (+ delta n)))))))
;;------------------------------------------------------------------------------
