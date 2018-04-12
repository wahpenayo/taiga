(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-11"
      :doc "Engel data affine regression models." }
    
    taiga.test.regress.engel.affine
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.engel.engel :as engel]))
;; mvn -Dtest=taiga.test.regress.engel.affine clojure:test > engel-affine.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(defn- test0 [^clojure.lang.IFn fit
              ^double true-translation
              ^doubles true-dual
              true-train-summary]
  (z/seconds 
    nss
    (let [options (assoc (engel/options)
                         :max-iterations 10000
                         :relative-tolerance 1.0e-8
                         :absolute-tolerance 1.0e-8
                         :line-search-relative-tolerance 1.0e-6
                         :line-search-absolute-tolerance 1.0e-6
                         :huber-epsilon 1.0e-3
                         :quantile-p 0.5
                         :start 
                         [0.5 100.0]
                         #_[0.5 100.0]
                         #_[1.0 1.0]
                         :gradient-check (z/print-writer System/out))
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
           (test/is (z/approximately== 1.0e-4 x0 x1))) 
         true-train-summary
         train-summary))))
;;----------------------------------------------------------------
(test/deftest affine-l1 
  (test0 taiga/affine-l1
         81.4822474
         (double-array [0.5601806])
         [-7.694427 120.3293 74.72312]))
;;----------------------------------------------------------------
#_(test/deftest affine-l2 
    (test0 taiga/affine-l2
           147.4754
           (double-array [0.4852])
           [-1.2743E-8 113.6213 77.3475]))
;;----------------------------------------------------------------
#_(test/deftest affine-l2-regression 
    (test0 taiga/affine-l2-regression
           147.4754
           (double-array [0.4852])
           [-1.2743E-8 113.6213 77.3475]))
;;----------------------------------------------------------------
