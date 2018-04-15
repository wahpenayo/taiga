(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-14"
      :doc 
      "Linear (no intercept) data and regression models." }
    
    taiga.test.regress.linear.l2
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.record :as record]
            [taiga.test.regress.data.defs :as defs])
  (:import [java.util Arrays]
           [taiga.test.regress.data.record Record]))
;; mvn -Dtest=taiga.test.regress.linear.l2 clojure:test > linear-l2.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
#_(test/deftest noiseless-linear-l2-regression
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [lf (record/make-linear-functional 10.0)
          ymean (fn ymean ^double [^Record datum]
                  (.invokePrim lf
                    (record/embedding record/xbindings datum)))
          options (assoc (defs/options 
                           record/attributes
                           record/xbindings
                           record/generator
                           ymean
                           -1.0)
                         :embedding record/embedding)
          model (taiga/linear-l2-regression options)
          _ (defs/edn-test model (defs/linear-edn-file nss))
          y (:ground-truth record/attributes)
          yhat (fn yhat ^double [datum] 
                 (model record/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))
          true-dual (z/dual lf)
          est-functional (taiga/functional model)
          est-dual (z/dual est-functional)]
      (z/mapc 
        (fn [^double bi ^double bihat]
          (test/is (z/approximately== 0.01 bi bihat))
          (print-str "not approximately==\n"
                     (z/pprint-str (into [] true-dual) 32)
                     "\n"
                     (z/pprint-str (into [] est-dual) 32)))
        (into [] true-dual) 
        (into [] est-dual))
      (test/is (= record/embedding (taiga/embedding model))
               (print-str "not =\n"
                          record/embedding "\n"
                          (taiga/embedding model)))
      (test/is (= [-1.0129005518951877E-12 9.140662574929177E-12 7.229843261657006E-12]
                  train-summary))
      (test/is (= [-9.43747407240994E-13 9.029374849844779E-12 7.133832781711264E-12]
                  test-summary)))))
;;----------------------------------------------------------------
#_(test/deftest noiseless-linear-l2
  (z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [lf (record/make-linear-functional 10.0)
          ymean (fn ymean ^double [^Record datum]
                  (.invokePrim lf
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
                         :line-search-relative-tolerance 1.0e-4
                         :line-search-absolute-tolerance 1.0e-4)
          model (taiga/linear-l2 options)
          _ (defs/edn-test model (defs/linear-edn-file nss))
          y (:ground-truth record/attributes)
          yhat (fn yhat ^double [datum] 
                 (model record/xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          _ (println "test:" )
          test-summary (defs/print-residual-summary 
                         y yhat (:test-data options))
          true-dual (z/dual lf)
          est-functional (taiga/functional model)
          est-dual (z/dual est-functional)]
      (z/mapc 
        (fn [^double bi ^double bihat]
          (test/is (z/approximately== 0.01 bi bihat))
          (print-str "not approximately==\n"
                     (z/pprint-str (into [] true-dual) 32)
                     "\n"
                     (z/pprint-str (into [] est-dual) 32)))
        (into [] true-dual) 
        (into [] est-dual))
      (test/is (= record/embedding (taiga/embedding model))
               (print-str "not =\n"
                          record/embedding "\n"
                          (taiga/embedding model)))
      (z/mapc 
         (fn [^double x0 ^double x1]
           (test/is (z/approximately== 1.0e-6 x0 x1))) 
         [-4.019611428811208E-10 1.0176596909008533E-9 6.916640757790045E-10]
         train-summary)
      (z/mapc 
        (fn [^double x0 ^double x1]
          (test/is (z/approximately== 1.0e-6 x0 x1))) 
        [-3.8753587189823195E-10 1.0018289672944002E-9 6.794233528695069E-10]
        test-summary))))
;;----------------------------------------------------------------
#_(test/deftest linear-l2
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [lf (record/make-linear-functional 10.0)
           ymean (fn ymean ^double [^Record datum]
                   (.invokePrim lf
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
                          :line-search-relative-tolerance 1.0e-4
                          :line-search-absolute-tolerance 1.0e-4)
           model (taiga/linear-l2 options)
           _ (defs/edn-test model (defs/linear-edn-file nss))
           y (:ground-truth record/attributes)
           yhat (fn yhat ^double [datum] 
                  (model record/xbindings datum))
           _ (println "train:" )
           train-summary (defs/print-residual-summary 
                           y yhat (:data options))
           _ (println "test:" )
           test-summary (defs/print-residual-summary 
                          y yhat (:test-data options))
           true-dual (z/dual lf)
           est-functional (taiga/functional model)
           est-dual (z/dual est-functional)]
       (println "true:\n" (z/pprint-str (into [] true-dual) 32))
       (println "est:\n" (z/pprint-str (into [] est-dual) 32))
       (z/mapc 
         (fn [^double bi ^double bihat]
           (test/is (z/approximately== 0.1 bi bihat))
           (print-str "not approximately==\n"
                      (z/pprint-str (into [] true-dual) 32)
                      "\n"
                      (z/pprint-str (into [] est-dual) 32)))
         (into [] true-dual) 
         (into [] est-dual))
       (test/is (= record/embedding (taiga/embedding model))
                (print-str "not =\n"
                           record/embedding "\n"
                           (taiga/embedding model)))
       (z/mapc 
         (fn [^double x0 ^double x1]
           (test/is (z/approximately== 1.0e-36 x0 x1))) 
         [-3.230517874782666E-4 0.5793063687780364 0.5015734236481969]
         train-summary)
       (z/mapc 
         (fn [^double x0 ^double x1]
           (test/is (z/approximately== 1.0e-6 x0 x1))) 
         [0.004919386939479624 0.578225989256591 0.5003394569826642]
         test-summary))))
;;----------------------------------------------------------------
#_(test/deftest linear-l2-regression
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [lf (record/make-linear-functional 10.0)
           ymean (fn ymean ^double [^Record datum]
                   (.invokePrim lf
                     (record/embedding record/xbindings datum)))
           options (assoc (defs/options 
                            record/attributes
                            record/xbindings
                            record/generator
                            ymean
                            2.0)
                          :embedding record/embedding)
           model (taiga/linear-l2-regression options)
           _ (defs/edn-test model (defs/linear-edn-file nss))
           y (:ground-truth record/attributes)
           yhat (fn yhat ^double [datum] 
                  (model record/xbindings datum))
           _ (println "train:" )
           train-summary (defs/print-residual-summary 
                           y yhat (:data options))
           _ (println "test:" )
           test-summary (defs/print-residual-summary 
                          y yhat (:test-data options))
           true-dual (z/dual lf)
           est-functional (taiga/functional model)
           est-dual (z/dual est-functional)]
       (println "true:\n" (z/pprint-str (into [] true-dual) 32))
       (println "est:\n" (z/pprint-str (into [] est-dual) 32))
       (z/mapc 
         (fn [^double bi ^double bihat]
           (test/is (z/approximately== 0.1 bi bihat))
           (print-str "not approximately==\n"
                      (z/pprint-str (into [] true-dual) 32)
                      "\n"
                      (z/pprint-str (into [] est-dual) 32)))
         (into [] true-dual) 
         (into [] est-dual))
       (test/is (= record/embedding (taiga/embedding model))
                (print-str "not =\n"
                           record/embedding "\n"
                           (taiga/embedding model)))
       (test/is (= [-3.2306342765455734E-4 0.5793063687780368 0.501573423587349]
                   train-summary))
       (test/is (= [0.00491937537242795 0.5782259891903178 0.5003394569861396]
                   test-summary)))))
;;----------------------------------------------------------------
#_(test/deftest noiseless-forest
   (z/reset-mersenne-twister-seeds)
   (z/seconds 
     nss
     (let [lf (record/make-linear-functional 10.0)
           ymean (fn ymean ^double [^Record datum]
                   (.invokePrim lf
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
                          y yhat (:test-data options))]
       (test/is (= (mapv taiga/node-height (taiga/terms model))
                   [13 12 13 12 13 12 13 12 12 12 13 12 12 12 13 12 13 12 12 13 12 13 12 12 12 13 13 12 12 12 12 14 12 13 13 13 12 12 12 12 12 13 13 12 13 12 12 12 13 12 12 12 12 12 12 12 12 13 13 13 12 12 12 12 12 12 14 13 13 13 13 12 12 12 13 13 12 12 13 12 12 12 12 12 12 13 12 12 12 13 12 12 12 12 13 12 13 14 12 12 12 12 12 13 13 12 13 13 13 12 12 12 12 12 12 12 12 12 12 12 12 13 13 12 12 12 12 12] 
                   ))
       (test/is (= (mapv taiga/count-children (taiga/terms model))
                   [381 385 385 377 387 385 389 383 383 389 391 381 395 379 379 379 377 381 381 377 383 389 387 381 389 383 381 389 381 373 381 397 379 389 397 377 387 385 383 377 385 389 379 391 383 381 373 387 397 385 391 391 375 387 381 385 385 383 373 389 383 387 375 383 389 375 379 381 377 381 379 387 371 381 385 397 387 379 397 379 387 383 387 383 377 391 375 393 389 377 387 371 385 387 385 393 385 389 393 381 387 383 381 397 383 383 377 381 397 387 375 383 385 377 381 387 389 393 377 377 387 397 383 387 393 383 387 381]))
       (test/is (= (mapv taiga/count-leaves (taiga/terms model))
                   [191 193 193 189 194 193 195 192 192 195 196 191 198 190 190 190 189 191 191 189 192 195 194 191 195 192 191 195 191 187 191 199 190 195 199 189 194 193 192 189 193 195 190 196 192 191 187 194 199 193 196 196 188 194 191 193 193 192 187 195 192 194 188 192 195 188 190 191 189 191 190 194 186 191 193 199 194 190 199 190 194 192 194 192 189 196 188 197 195 189 194 186 193 194 193 197 193 195 197 191 194 192 191 199 192 192 189 191 199 194 188 192 193 189 191 194 195 197 189 189 194 199 192 194 197 192 194 191]))
       (test/is (= [-0.03986470431795581 30.173563294042975 23.766694844693262] 
                   train-summary))
       (test/is (= [-0.11451523218176905 31.444713301662027 24.896134282701638]
                   test-summary)))))
;;----------------------------------------------------------------
(test/deftest forest
 (z/reset-mersenne-twister-seeds)
 (z/seconds 
   nss
   (let [lf (record/make-linear-functional 10.0)
         ymean (fn ymean ^double [^Record datum]
                 (.invokePrim lf
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
                        y yhat (:test-data options))]
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
     (test/is (= [-0.03434293246041941 30.22116788396509 23.802337006769605]
                 train-summary))
     (test/is (= [-0.09751446238091328 31.482015010407466 24.91840942313281]
                 test-summary)))))
;;----------------------------------------------------------------