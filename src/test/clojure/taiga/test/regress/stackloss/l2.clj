(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-05"
      :doc "Stackloss l2 regression models." }
    
    taiga.test.regress.stackloss.l2
  
  (:require [clojure.java.io :as io]
            [clojure.test :as test]
            [zana.api :as z]
            [taiga.api :as taiga]
            [taiga.test.tree :as tree]
            [taiga.test.regress.data.defs :as defs]
            [taiga.test.regress.stackloss.stackloss :as stackloss]))
;; mvn -Dtest=taiga.test.regress.stackloss.l2 clojure:test > stackloss-l2.txt
;;----------------------------------------------------------------
(def nss (str *ns*))
;;----------------------------------------------------------------
(test/deftest affine-l2
  #_(z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (stackloss/options)
          model (taiga/affine-l2 options)
          _ (defs/edn-test model (defs/affine-edn-file nss))
          y (:ground-truth (:attributes options))
          xbindings (into (sorted-map)
                          (dissoc (:attributes options) 
                                  :ground-truth :prediction))
          yhat (fn yhat ^double [datum] (model xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          true-dual (double-array [0.7156 1.2953 -0.1521])
          true-translation -39.9197
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
      (z/mapc 
        (fn [^double bi ^double bihat]
          (test/is (z/approximately== 1.0e-4 bi bihat))
          (print-str "not approximately==\n"
                     (z/pprint-str (into [] true-dual) 32)
                     "\n"
                     (z/pprint-str (into [] est-dual) 32)))
        (into [] true-dual) 
        (into [] est-dual))
      (z/mapc 
        (fn [^double x0 ^double x1]
          (test/is (z/approximately== 1.0e-6 x0 x1))) 
        [2.0111546713711244E-8 2.918169367439959 2.366620201019657]
        train-summary))))
;;----------------------------------------------------------------
(test/deftest affine-l2-regression
  #_(z/reset-mersenne-twister-seeds)
  (z/seconds 
    nss
    (let [options (stackloss/options)
          model (taiga/affine-l2-regression options)
          _ (defs/edn-test model (defs/affine-edn-file nss))
          y (:ground-truth (:attributes options))
          xbindings (into (sorted-map)
                          (dissoc (:attributes options) 
                                  :ground-truth :prediction))
          yhat (fn yhat ^double [datum] (model xbindings datum))
          _ (println "train:" )
          train-summary (defs/print-residual-summary 
                          y yhat (:data options))
          true-dual (double-array [0.7156 1.2953 -0.1521])
          true-translation -39.9197
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
      (z/mapc 
        (fn [^double bi ^double bihat]
          (test/is (z/approximately== 1.0e-4 bi bihat))
          (print-str "not approximately==\n"
                     (z/pprint-str (into [] true-dual) 32)
                     "\n"
                     (z/pprint-str (into [] est-dual) 32)))
        (into [] true-dual) 
        (into [] est-dual))
      (z/mapc 
        (fn [^double x0 ^double x1]
          (test/is (z/approximately== 1.0e-6 x0 x1))) 
        [2.0111546713711244E-8 2.918169367439959 2.366620201019657]
        train-summary))))
;;----------------------------------------------------------------
#_(test/deftest forest
    (z/reset-mersenne-twister-seeds)
    (z/seconds 
      nss
      (let [af (stackloss/make-affine-functional 10.0)
            ymean (fn ymean ^double [^Record datum]
                    (.invokePrim af
                      (stackloss/embedding stackloss/xbindings datum)))
            options (defs/options 
                      stackloss/attributes
                      stackloss/xbindings
                      stackloss/generator
                      ymean
                      2.0)
            model (taiga/mean-regression options)
            _ (z/mapc #(tree/check-mincount options %) 
                      (taiga/terms model))
            _ (defs/json-test nss options model)
            _ (defs/edn-test 
                model (defs/forest-file nss options model))
            y (:ground-truth stackloss/attributes)
            yhat (fn yhat ^double [datum] 
                   (model stackloss/xbindings datum))
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
        (test/is (= [-0.03434293246041948 
                     30.22116788396509
                     23.802337006769612] 
                    train-summary))
        (test/is (= [-0.09751446238091321
                     31.482015010407466
                     24.91840942313281]
                    test-summary)))))
;;----------------------------------------------------------------