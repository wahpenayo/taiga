(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-13"
      :doc "Engel data constant regression models." }
    
    taiga.test.regress.engel.constant
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.engel.engel :as engel]))
;; mvn -Dtest=taiga.test.regress.engel.constant clojure:test > engel-constant.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(defn- test0 [^clojure.lang.IFn fit
              ^double true-translation
              ^doubles true-dual
              true-train-summary]
  (z/seconds 
    nss
    (let [options (assoc
                    (engel/options)
                    :attributes {:ground-truth engel/foodexp
                                 :prediction engel/predicted-foodexp}
                    :embedding (z/affine-embedding "engel" [])
                    :minimize? true
                    :max-iterations 10000
                    :initial-bracket-range 1.0e-3
                    :relative-tolerance 1.0e-6
                    :absolute-tolerance 1.0e-6
                    :line-search-relative-tolerance 1.0e-3
                    :line-search-absolute-tolerance 1.0e-3
                    :huber-epsilon 1.0e0
                    :quantile-p 0.5
                    :start
                    [0]
                    #_[0.5601806 81.4822474]
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
      (test/is (z/approximately== 
                 1.0e-4 true-translation est-translation)
               (print-str "not ==\n"
                          true-translation "\n"
                          est-translation))
      (z/mapc 
        (fn [^double x0 ^double x1]
          (test/is (z/approximately== 1.0e-4 x0 x1))) 
        true-train-summary
        train-summary))))
;;----------------------------------------------------------------
(test/deftest constant-l1 
  (test0 taiga/affine-l1
         582.5413
         (double-array 0)
         [41.60886 278.9884 196.9279]))
;;----------------------------------------------------------------
(test/deftest affine-l2 
   (test0 taiga/affine-l2
           624.1501
          (double-array 0)
          [-1.2743E-8 275.8682 200.4251]))
;;----------------------------------------------------------------
(test/deftest affine-l2-regression 
   (test0 taiga/affine-l2-regression
          624.1501
          (double-array 0)
          [-1.2743E-8 275.8682 200.4251]))
;;----------------------------------------------------------------
