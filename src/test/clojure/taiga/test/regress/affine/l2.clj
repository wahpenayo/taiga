(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-17"
      :doc 
      "Affine data and regression models." }
    
    taiga.test.regress.affine.l2
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.record :as record]
            [taiga.test.regress.data.defs :as defs])
  (:import [java.util Arrays]
           [taiga.test.regress.data.record Record]))
;; mvn -Dtest=taiga.test.regress.affine.l2 clojure:test > affine-l2.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(test/deftest noiseless-affine-l2
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
                          :relative-tolerance 1.0e-7
                          :absolute-tolerance 1.0e-7
                          :line-search-relative-tolerance 1.0e-5
                          :line-search-absolute-tolerance 1.0e-5)
           model (taiga/affine-l2 options)
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
                   :rmqr 2.416719328834655E-9,
                   :rmrq 2.416719328834655E-9}
                  train-summary))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean -5.079852711436031E-11,
                   :rmse 3.043432716952257E-9,
                   :rmad 2.386150001274911E-9,
                   :rmqr 2.386150001274911E-9,
                   :rmrq 2.386150001274911E-9}
                  test-summary)))))
;;----------------------------------------------------------------
(test/deftest affine-l2
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
                          :relative-tolerance 1.0e-7
                          :absolute-tolerance 1.0e-7
                          :line-search-relative-tolerance 1.0e-6
                          :line-search-absolute-tolerance 1.0e-6)
           model (taiga/affine-l2 options)
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
                   :rmqr 0.5015659845053395,
                   :rmrq 0.5015659845053395}
                  train-summary))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean 0.00527143350536167,
                   :rmse 0.5782203557975366,
                   :rmad 0.500335754126387,
                   :rmqr 0.500335754126387,
                   :rmrq 0.500335754126387}
                  test-summary)))))
;;----------------------------------------------------------------
(test/deftest noiseless-affine-l2-regression
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
                          :embedding record/embedding)
           model (taiga/affine-l2-regression options)
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
       (test/is 
         (z/maps-approximately== 
           ulps
           {:rmean 2.2575756368492517E-13,
            :rmse 1.3026765957732521E-11,
            :rmad 9.249832779063827E-12,
            :rmqr 9.249832779063827E-12,
            :rmrq 9.249832779063827E-12}
           train-summary))
       (test/is 
         (z/maps-approximately== 
           ulps
           {:rmean 2.3659188757435556E-14,
            :rmse 1.278172279594286E-11,
            :rmad 9.063964043296707E-12,
            :rmqr 9.063964043296707E-12,
            :rmrq 9.063964043296707E-12}
           test-summary)))))
;;----------------------------------------------------------------
(test/deftest affine-l2-regression
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
                          :embedding record/embedding)
           model (taiga/affine-l2-regression options)
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
                   :rmqr 0.5015659845053395,
                   :rmrq 0.5015659845053395}
                  train-summary))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean 0.00527143350536167,
                   :rmse 0.5782203557975366,
                   :rmad 0.500335754126387,
                   :rmqr 0.500335754126387,
                   :rmrq 0.500335754126387}
                  test-summary)))))
;;----------------------------------------------------------------
(test/deftest noiseless-forest
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [af (record/make-affine-functional 10.0)
           ymean (fn ymean ^double [^Record datum]
                   (.invokePrim af
                     (record/embedding record/xbindings datum)))
           options (defs/options 
                     record/attributes
                     record/xbindings
                     record/generator
                     ymean
                     -1.0)
           model (taiga/mean-regression options)
           _ (z/mapc #(tree/check-mincount options %) 
                     (taiga/terms model))
           _ (defs/json-test nss options model)
           _ (defs/edn-test 
               model (defs/forest-file nss options model))
           y (:ground-truth record/attributes)
           yhat (fn yhat ^double [datum] 
                  (model record/xbindings datum))
           _ (println "train:" )
           train-summary (defs/print-residual-summary 
                           y yhat (:data options))
           _ (println "test:" )
           test-summary (defs/print-residual-summary 
                          y yhat (:test-data options))
           ulps 1.0]
       (test/is (= (mapv taiga/node-height (taiga/terms model))
                   [13 12 13 12 13 12 13 12 12 12 13 12 12 12 13 12 13 12 12 13 12 13 12 12 12 13 13 12 12 12 12 14 12 13 13 13 12 12 12 12 12 13 13 12 13 12 12 12 13 12 12 12 12 12 12 12 12 13 13 13 12 12 12 12 12 12 14 13 13 13 13 12 12 12 13 13 12 12 13 12 12 12 12 12 12 13 12 12 12 13 12 12 12 12 13 12 13 14 12 12 12 12 12 13 13 12 13 13 13 12 12 12 12 12 12 12 12 12 12 12 12 13 13 12 12 12 12 12] 
                   ))
       (test/is (= (mapv taiga/count-children (taiga/terms model))
                   [381 385 385 377 387 385 389 383 383 389 391 381 395 379 379 379 377 381 381 377 383 389 387 381 389 383 381 389 381 373 381 397 379 389 397 377 387 385 383 377 385 389 379 391 383 381 373 387 397 385 391 391 375 387 381 385 385 383 373 389 383 387 375 383 389 375 379 381 377 381 379 387 371 381 385 397 387 379 397 379 387 383 387 383 377 391 375 393 389 377 387 371 385 387 385 393 385 389 393 381 387 383 381 397 383 383 377 381 397 387 375 383 385 377 381 387 389 393 377 377 387 397 383 387 393 383 387 381]))
       (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                   [191 193 193 189 194 193 195 192 192 195 196 191 198 190 190 190 189 191 191 189 192 195 194 191 195 192 191 195 191 187 191 199 190 195 199 189 194 193 192 189 193 195 190 196 192 191 187 194 199 193 196 196 188 194 191 193 193 192 187 195 192 194 188 192 195 188 190 191 189 191 190 194 186 191 193 199 194 190 199 190 194 192 194 192 189 196 188 197 195 189 194 186 193 194 193 197 193 195 197 191 194 192 191 199 192 192 189 191 199 194 188 192 193 189 191 194 195 197 189 189 194 199 192 194 197 192 194 191]))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean -0.03986470431795575,
                   :rmse 30.173563294042975,
                   :rmad 23.766694844693262,
                   :rmqr 23.766694844693262,
                   :rmrq 23.766694844693262}
                  train-summary))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean -0.11451523218176911,
                   :rmse 31.44471330166203,
                   :rmad 24.896134282701638,
                   :rmqr 24.896134282701638,
                   :rmrq 24.896134282701638}
                  test-summary)))))
;;----------------------------------------------------------------
(test/deftest forest
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [af (record/make-affine-functional 10.0)
           ymean (fn ymean ^double [^Record datum]
                   (.invokePrim af
                     (record/embedding record/xbindings datum)))
           options (defs/options 
                     record/attributes
                     record/xbindings
                     record/generator
                     ymean
                     2.0)
           model (taiga/mean-regression options)
           _ (z/mapc #(tree/check-mincount options %) 
                     (taiga/terms model))
           _ (defs/json-test nss options model)
           _ (defs/edn-test 
               model (defs/forest-file nss options model))
           y (:ground-truth record/attributes)
           yhat (fn yhat ^double [datum] 
                  (model record/xbindings datum))
           _ (println "train:" )
           train-summary (defs/print-residual-summary 
                           y yhat (:data options))
           _ (println "test:" )
           test-summary (defs/print-residual-summary 
                          y yhat (:test-data options))
           ulps 1.0]
       (test/is (= (mapv taiga/node-height (taiga/terms model))
                   [12 12 13 12 13 12 13 12 12 12 13 12 12 12 13 12
                    13 12 12 12 12 13 12 12 12 13 13 12 12 12 12 14
                    12 13 12 13 12 12 12 12 12 13 13 13 12 12 12 12
                    13 12 12 12 12 12 12 12 12 13 12 13 12 12 12 12
                    13 12 14 13 12 13 13 12 12 12 13 13 12 12 13 12
                    12 13 12 12 12 12 12 12 12 13 12 12 12 12 12 12
                    13 14 13 12 12 12 12 13 12 12 13 13 13 12 12 12
                    12 13 12 12 12 12 12 12 12 12 13 12 12 12 12 12]))
       (test/is (= (mapv taiga/count-children (taiga/terms model))
                   [385 385 373 377 387 385 391 373 387 389 391 381
                    387 379 379 381 377 381 381 385 383 389 393 381
                    389 383 377 389 381 383 381 397 379 387 385 377
                    391 385 383 377 385 389 379 389 379 381 373 375
                    397 385 391 391 379 387 381 385 381 383 391 389
                    387 387 375 375 385 375 379 385 379 381 379 387
                    371 381 383 379 385 377 397 379 381 381 389 383
                    375 379 375 393 389 377 385 371 385 387 385 393
                    385 389 381 385 387 381 381 395 377 383 379 381
                    397 383 375 383 385 379 381 385 389 395 381 377
                    385 391 383 387 393 383 387 381]))
       (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                   [193 193 187 189 194 193 196 187 194 195 196 191
                    194 190 190 191 189 191 191 193 192 195 197 191
                    195 192 189 195 191 192 191 199 190 194 193 189
                    196 193 192 189 193 195 190 195 190 191 187 188 
                    199 193 196 196 190 194 191 193 191 192 196 195
                    194 194 188 188 193 188 190 193 190 191 190 194
                    186 191 192 190 193 189 199 190 191 191 195 192
                    188 190 188 197 195 189 193 186 193 194 193 197
                    193 195 191 193 194 191 191 198 189 192 190 191
                    199 192 188 192 193 190 191 193 195 198 191 189
                    193 196 192 194 197 192 194 191]))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean -0.03434293246041948,
                   :rmse 30.22116788396509,
                   :rmad 23.802337006769612,
                   :rmqr 23.802337006769612,
                   :rmrq 23.802337006769612}
                  train-summary))
       (test/is (z/maps-approximately== 
                  ulps
                  {:rmean -0.09751446238091321,
                   :rmse 31.482015010407466,
                   :rmad 24.91840942313281,
                   :rmqr 24.91840942313281,
                   :rmrq 24.91840942313281}
                  test-summary)))))
;;----------------------------------------------------------------