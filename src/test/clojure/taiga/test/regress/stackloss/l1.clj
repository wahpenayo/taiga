(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-06"
      :doc "Stackloss l1 regression models." }
    
    taiga.test.regress.stackloss.l1
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.stackloss.stackloss :as stackloss]))
;; mvn -Dtest=taiga.test.regress.stackloss.l1 clojure:test > stackloss-l1.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
(defn- l1-test [fit]
  (z/seconds 
    nss
    (let [options (assoc (stackloss/options)
                          :max-iterations 10000
                          :relative-tolerance 1.0e-8
                          :absolute-tolerance 1.0e-8
                          :line-search-relative-tolerance 1.0e-4
                          :line-search-absolute-tolerance 1.0e-4
                          :huber-epsilon 4.0e-3
                          ;;:quantile-p 0.5
                          :start 
                          #_[1.0 1.0 1.0 1.0]
                          [-0.04795871846048864 0.8335442927793958 0.5661022224622854
                            -40.74690955003594])
          model (fit options)
          _ (defs/edn-test model (defs/affine-edn-file nss))
          y (:ground-truth (:attributes options))
          xbindings (into (sorted-map)
                          (dissoc (:attributes options) 
                                  :ground-truth :prediction))
          yhat (fn yhat ^double [datum] (model xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          true-dual (double-array [-0.06086957 0.83188406 0.57391304])
          true-translation -39.68985507
          est-functional (taiga/functional model)
          est-dual (z/dual (z/linear-part est-functional))
          est-translation (z/translation est-functional)]
      (println "tru:" true-translation)
      (println "est:" est-translation)
      (println "true:\n" (z/pprint-str (into [] true-dual) 32))
      (println "est:\n" (z/pprint-str (into [] est-dual) 32))
      (test/is (z/approximately== 
                 1.0e-4 true-translation est-translation)
               (print-str "not ==\n"
                          true-translation "\n"
                          est-translation))
      #_(z/mapc 
         (fn [^double bi ^double bihat]
           (test/is (z/approximately== 1.0e-4 bi bihat))
           (print-str "not approximately==\n"
                      (z/pprint-str (into [] true-dual) 32)
                      "\n"
                      (z/pprint-str (into [] est-dual) 32)))
         (into [] true-dual) 
         (into [] est-dual))
      #_(z/mapc 
         (fn [^double x0 ^double x1]
           (test/is (z/approximately== 1.0e-6 x0 x1))) 
         [2.0111546713711244E-8 2.918169367439959 2.366620201019657]
         train-summary))))
;;----------------------------------------------------------------
(test/deftest affine-l1
  (l1-test taiga/affine-l1))
;;----------------------------------------------------------------