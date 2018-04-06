(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-04-05"
      :doc 
      "Affine data and regression models." }
    
    taiga.test.regress.engel.qr
  
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
(test/deftest noiseless-affine-q75
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
          est-translation (z/translation est-functional)]
      (test/is (z/approximately== 
                 1.0 true-translation est-translation)
               (print-str "not ==\n"
                          true-translation "\n"
                          est-translation))
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
          (test/is (z/approximately== 1.0e-3 x0 x1))) 
        [-0.2529624499597556 0.38468973830600833 0.31460321825150517]
        train-summary)
      (z/mapc 
        (fn [^double x0 ^double x1]
          (test/is (z/approximately== 1.0e-3 x0 x1))) 
        [-0.2556904768448136 0.38606871853980673 0.3160543799261435]
        test-summary))))
;;----------------------------------------------------------------
(test/deftest affine-q75
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
                           :max-iterations 10000
                           :quantile-p 0.75
                           :huber-epsilon 1.0e-4
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
            est-translation (z/translation est-functional)]
        (test/is (z/approximately== 
                   1.0 true-translation est-translation)
                 (print-str "not ==\n"
                            true-translation "\n"
                            est-translation))
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
            (test/is (z/approximately== 1.0e-1 x0 x1))) 
          [-0.49981398420952317 0.7652445396047906 0.6259540593979588]
          train-summary)
        (z/mapc 
          (fn [^double x0 ^double x1]
            (test/is (z/approximately== 1.0e-1 x0 x1))) 
          [-0.49453659785573656 0.7608797624683304 0.6227600789007163]
          test-summary))))
;;----------------------------------------------------------------
(test/deftest noiseless-affine-l1
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
            est-translation (z/translation est-functional)]
        (test/is (z/approximately== 
                   0.1 true-translation est-translation)
                 (print-str "not ==\n"
                            true-translation "\n"
                            est-translation))
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
            (test/is (z/approximately== 1.0e-3 x0 x1))) 
          [2.2052744016749087E-13 
           0.2896521377538011
           0.25078299164728407]
          train-summary)
        (z/mapc 
          (fn [^double x0 ^double x1]
            (test/is (z/approximately== 1.0e-3 x0 x1))) 
          [-0.002635712379207253
           0.2891101777340187
           0.2501678760956035]
          test-summary))))
;;----------------------------------------------------------------
(test/deftest affine-l1
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
            est-translation (z/translation est-functional)]
        (test/is (z/approximately== 
                   0.1 true-translation est-translation)
                 (print-str "not ==\n"
                            true-translation "\n"
                            est-translation))
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
            (test/is (z/approximately== 1.0e-3 x0 x1))) 
          [5.570680364479913E-13 
           0.5793042755076032 
           0.5015659832944205]
          train-summary)
        (z/mapc 
          (fn [^double x0 ^double x1]
            (test/is (z/approximately== 1.0e-3 x0 x1))) 
          [0.005271424758805213
           0.578220355468474
           0.5003357521916836]
          test-summary))))
;;----------------------------------------------------------------
