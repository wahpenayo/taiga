(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2018-08-17"
      :doc "Debug pyramid function probability forest example." }
    
    taiga.scripts.debug.pyramid
  
  (:require [clojure.test :as test]
            [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.api :as taiga]
            [taiga.scripts.data.record :as record]
            [taiga.scripts.data.defs :as defs])
  
  (:import [clojure.lang IFn$OD]))
;; mvn clean install -DskipTests
;; clj src/scripts/clojure/taiga/scripts/debug/pyramid.clj > debug.txt
;;----------------------------------------------------------------
(z/reset-mersenne-twister-seeds)
(let [options (assoc
                (defs/options 
                  (record/make-pyramid-function 1.0) 1024)
                :nterms 2)
      forest (taiga/positive-fraction-probability options)
      predictors (into (sorted-map)
                       (dissoc record/attributes 
                               :ground-truth :prediction))
      ^IFn$OD phat (fn phat ^double [datum] 
                     (.invokePrim forest predictors datum))]
  (binding [utils/*debug* true]
    (phat (first (:test-data options)))))
