(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-16"
      :doc 
      "Affine data and regression models." }
    
    taiga.test.regress.affine.qr
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.record :as record]
            [taiga.test.regress.data.defs :as defs])
  (:import [java.util Arrays]
           [taiga.test.regress.data.record Record]))
;; mvn -Dtest=taiga.test.regress.affine.qr clojure:test > affine-qr.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
#_(test/deftest noiseless-affine-l1
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [af (record/make-affine-functional 10.0)
           ymean (fn ymean ^double [^Record datum]
                   (.invokePrim af
                     (record/embedding record/xbindings datum)))
           options (assoc (defs/options 
                            record/attributes
                            record/xbindings
                            record/generator
                            ymean
                            -1.0)
                          :embedding record/embedding
                          :max-iterations 10000
                          :quantile-p 0.5
                          :huber-epsilon 1.0e-3
                          :relative-tolerance 1.0e-6
                          :absolute-tolerance 1.0e-6
                          :line-search-relative-tolerance 1.0e-4
                          :line-search-absolute-tolerance 1.0e-4)
           model (taiga/affine-qr options)
           _ (defs/edn-test model (defs/affine-edn-file nss))
           y (:ground-truth record/attributes)
           yhat (fn yhat ^double [datum] 
                  (model record/xbindings datum))
           _ (println "train:" )
           train-summary (defs/print-residual-summary 
                           y yhat (:data options))
           _ (println "test:" )
           test-summary (defs/print-residual-summary 
                          y yhat (:test-data options))
           true-dual (z/dual (z/linear-part af))
           true-translation (z/translation af)
           est-functional (taiga/functional model)
           est-dual (z/dual (z/linear-part est-functional))
           est-translation (z/translation est-functional)
           ulps 2.0e4]
       (test/is 
         (z/approximately== 
           1.0e3 true-translation est-translation)
         (print-str "not ==\n"
                    true-translation "\n"
                    est-translation))
       (test/is 
         (z/doubles-approximately== 5.0e5 true-dual est-dual)
         (print-str "not approximately==\n"
                    (z/pprint-str (into [] true-dual) 32)
                    "\n"
                    (z/pprint-str (into [] est-dual) 32)))
       (test/is (= record/embedding (taiga/embedding model))
                (print-str "not =\n"
                           record/embedding "\n"
                           (taiga/embedding model)))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean -3.7810618893664716E-12,
                   :rmse 3.0758255546068227E-9,
                   :rmad 2.416719328834655E-9,
                   :rmqr 1.2083596644173276E-9,
                   :rmrq 4.83343865766931E-9}
                  train-summary))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean -5.079852711436031E-11,
                   :rmse 3.043432716952257E-9,
                   :rmad 2.386150001274911E-9,
                   :rmqr 1.1930750006374554E-9,
                   :rmrq 4.772300002549822E-9}
                  test-summary)))))
;;----------------------------------------------------------------
#_(test/deftest affine-l1
    (z/reset-mersenne-twister-seeds)
    (z/seconds 
      nss
      (let [af (record/make-affine-functional 10.0)
            ymean (fn ymean ^double [^Record datum]
                    (.invokePrim af
                      (record/embedding record/xbindings datum)))
            options (assoc (defs/options 
                             record/attributes
                             record/xbindings
                             record/generator
                             ymean
                             2.0)
                           :embedding record/embedding
                           :quantile-p 0.5
                           :huber-epsilon 1.0e-3
                           :max-iterations 10000
                           :relative-tolerance 1.0e-6
                           :absolute-tolerance 1.0e-6
                           :line-search-relative-tolerance 1.0e-4
                           :line-search-absolute-tolerance 1.0e-4)
            model (taiga/affine-qr options)
            _ (defs/edn-test model (defs/affine-edn-file nss))
            y (:ground-truth record/attributes)
            yhat (fn yhat ^double [datum] 
                   (model record/xbindings datum))
            _ (println "train:" )
            train-summary (defs/print-residual-summary 
                            y yhat (:data options))
            _ (println "test:" )
            test-summary (defs/print-residual-summary 
                           y yhat (:test-data options))
            true-dual (z/dual (z/linear-part af))
            true-translation (z/translation af)
            est-functional (taiga/functional model)
            est-dual (z/dual (z/linear-part est-functional))
            est-translation (z/translation est-functional)
            ulps 5.0e7]
        (println "tru:" true-translation)
        (println "est:" est-translation)
        (println "true:\n" (z/pprint-str (into [] true-dual) 32))
        (println "est:\n" (z/pprint-str (into [] est-dual) 32))
        (test/is (z/approximately== 
                   5.0e12 true-translation est-translation)
                 (print-str "not ==\n"
                            true-translation "\n"
                            est-translation))
        (test/is 
          (z/doubles-approximately== 1.0e13 true-dual est-dual)
          (print-str "not approximately==\n"
                     (z/pprint-str (into [] true-dual) 32)
                     "\n"
                     (z/pprint-str (into [] est-dual) 32)))
        (test/is (= record/embedding (taiga/embedding model))
                 (print-str "not =\n"
                            record/embedding "\n"
                            (taiga/embedding model)))
        (test/is (z/maps-approximately== 
                   ulps
                   {:rmean 6.912143451712075E-9,
                    :rmse 0.5793042755076772,
                    :rmad 0.5015659845053395,
                    :rmqr 0.25078299225266976,
                    :rmrq 1.003131969010679}
                   train-summary))
        (test/is (z/maps-approximately== 
                   ulps
                   {:rmean 0.00527143350536167,
                    :rmse 0.5782203557975366,
                    :rmad 0.500335754126387,
                    :rmqr 0.2501678770631935,
                    :rmrq 1.000671508252774}
                   test-summary)))))
;;----------------------------------------------------------------
#_(test/deftest noiseless-affine-q75
    (z/reset-mersenne-twister-seeds)
    (z/seconds 
      nss
      (let [af (record/make-affine-functional 10.0)
            ymean (fn ymean ^double [^Record datum]
                    (.invokePrim af
                      (record/embedding record/xbindings datum)))
            options (assoc (defs/options 
                             record/attributes
                             record/xbindings
                             record/generator
                             ymean
                             -1.0)
                           :embedding record/embedding
                           :max-iterations 10000
                           :quantile-p 0.75
                           :huber-epsilon 1.0e-3
                           :relative-tolerance 1.0e-7
                           :absolute-tolerance 1.0e-7
                           :line-search-relative-tolerance 1.0e-4
                           :line-search-absolute-tolerance 1.0e-4)
            model (taiga/affine-qr options)
            _ (defs/edn-test model (defs/affine-edn-file nss))
            y (:ground-truth record/attributes)
            yhat (fn yhat ^double [datum] 
                   (model record/xbindings datum))
            _ (println "train:" )
            train-summary (defs/print-residual-summary 
                            y yhat (:data options))
            _ (println "test:" )
            test-summary (defs/print-residual-summary 
                           y yhat (:test-data options))
            true-dual (z/dual (z/linear-part af))
            true-translation (z/translation af)
            est-functional (taiga/functional model)
            est-dual (z/dual (z/linear-part est-functional))
            est-translation (z/translation est-functional)
            ulps 2.0e4]
        (test/is 
          (z/approximately== 
            1.0e3 true-translation est-translation)
          (print-str "not ==\n"
                     true-translation "\n"
                     est-translation))
        (test/is 
          (z/doubles-approximately== 5.0e5 true-dual est-dual)
          (print-str "not approximately==\n"
                     (z/pprint-str (into [] true-dual) 32)
                     "\n"
                     (z/pprint-str (into [] est-dual) 32)))
        (test/is (= record/embedding (taiga/embedding model))
                 (print-str "not =\n"
                            record/embedding "\n"
                            (taiga/embedding model)))
        (test/is (z/maps-approximately== 
                   ulps
                   {:rmean -3.7810618893664716E-12,
                    :rmse 3.0758255546068227E-9,
                    :rmad 2.416719328834655E-9,
                    :rmqr 1.2083596644173276E-9,
                    :rmrq 4.83343865766931E-9}
                   train-summary))
        (test/is (z/maps-approximately== 
                   ulps
                   {:rmean -5.079852711436031E-11,
                    :rmse 3.043432716952257E-9,
                    :rmad 2.386150001274911E-9,
                    :rmqr 1.1930750006374554E-9,
                    :rmrq 4.772300002549822E-9}
                   test-summary)))))
;;----------------------------------------------------------------
#_(test/deftest affine-q75
    (z/reset-mersenne-twister-seeds)
    (z/seconds 
      nss
      (let [af (record/make-affine-functional 10.0)
            ymean (fn ymean ^double [^Record datum]
                    (.invokePrim af
                      (record/embedding record/xbindings datum)))
            options (assoc (defs/options 
                             record/attributes
                             record/xbindings
                             record/generator
                             ymean
                             2.0)
                           :embedding record/embedding
                           :quantile-p 0.75
                           :huber-epsilon 1.0e-4
                           :max-iterations 10000
                           :relative-tolerance 1.0e-6
                           :absolute-tolerance 1.0e-6
                           :line-search-relative-tolerance 1.0e-4
                           :line-search-absolute-tolerance 1.0e-4)
            model (taiga/affine-qr options)
            _ (defs/edn-test model (defs/affine-edn-file nss))
            y (:ground-truth record/attributes)
            yhat (fn yhat ^double [datum] 
                   (model record/xbindings datum))
            _ (println "train:" )
            train-summary (defs/print-residual-summary 
                            y yhat (:data options))
            _ (println "test:" )
            test-summary (defs/print-residual-summary 
                           y yhat (:test-data options))
            true-dual (z/dual (z/linear-part af))
            true-translation (z/translation af)
            est-functional (taiga/functional model)
            est-dual (z/dual (z/linear-part est-functional))
            est-translation (z/translation est-functional)
            ulps 5.0e7]
        (println "tru:" true-translation)
        (println "est:" est-translation)
        (println "true:\n" (z/pprint-str (into [] true-dual) 32))
        (println "est:\n" (z/pprint-str (into [] est-dual) 32))
        (test/is (z/approximately== 
                   5.0e12 true-translation est-translation)
                 (print-str "not ==\n"
                            true-translation "\n"
                            est-translation))
        (test/is 
          (z/doubles-approximately== 1.0e13 true-dual est-dual)
          (print-str "not approximately==\n"
                     (z/pprint-str (into [] true-dual) 32)
                     "\n"
                     (z/pprint-str (into [] est-dual) 32)))
        (test/is (= record/embedding (taiga/embedding model))
                 (print-str "not =\n"
                            record/embedding "\n"
                            (taiga/embedding model)))
        (test/is (z/maps-approximately== 
                   ulps
                   {:rmean 6.912143451712075E-9,
                    :rmse 0.5793042755076772,
                    :rmad 0.5015659845053395,
                    :rmqr 0.25078299225266976,
                    :rmrq 1.003131969010679}
                   train-summary))
        (test/is (z/maps-approximately== 
                   ulps
                   {:rmean 0.00527143350536167,
                    :rmse 0.5782203557975366,
                    :rmad 0.500335754126387,
                    :rmqr 0.2501678770631935,
                    :rmrq 1.000671508252774}
                   test-summary)))))
;;----------------------------------------------------------------
