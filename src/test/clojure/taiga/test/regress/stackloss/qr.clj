(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-05"
      :doc 
      "Affine data and regression models." }
    
    taiga.test.regress.stackloss.qr
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.stackloss.stackloss :as stackloss]))
;; mvn -Dtest=taiga.test.regress.stackloss.qr clojure:test > stackloss-qr.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(test/deftest affine-l1
  #_(z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (assoc (stackloss/options)
                         :max-iterations 10000
                         :relative-tolerance 1.0e-8
                         :absolute-tolerance 1.0e-8
                         :line-search-relative-tolerance 1.0e-6
                         :line-search-absolute-tolerance 1.0e-6
                         :huber-epsilon 1.0e-6
                         :quantile-p 0.5
                         :start 
                         [0.8 0.6 -0.1 -40.0]
                         #_[0.83188406 0.57391304 -0.06086957
                           -39.68985507])
          model (taiga/affine-qr options)
          _ (defs/edn-test model (defs/affine-edn-file nss))
          y (:ground-truth (:attributes options))
          xbindings (into (sorted-map)
                          (dissoc (:attributes options) 
                                  :ground-truth :prediction))
          yhat (fn yhat ^double [datum] (model xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          true-dual (double-array 
                      [0.83188406 0.57391304 -0.06086957])
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
         [2.0111546713711244E-8 
          2.918169367439959 
          2.366620201019657]
         train-summary))))
;;----------------------------------------------------------------
;; q75
;; -5.418966e+01 :translation     
;; 8.706897e-01      9.827586e-01      2.677979e-16 :dual