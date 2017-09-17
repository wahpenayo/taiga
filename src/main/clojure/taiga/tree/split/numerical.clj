(set! *warn-on-reflection* true)
(set! *unchecked-math* false)
(ns ^{:author "John Alan McDonald, Kristina Lisa Klinkner" :date "2016-11-29"
      :doc "Greedy decision tree splitting." }
    
    taiga.tree.split.numerical
  
  (:require [taiga.tree.split.numericalleft :as left]
            [taiga.tree.split.numericalright :as right]))
;;------------------------------------------------------------------------------
;; TODO: left and right weight decide which way to go on missing data
;; TODO: put somewhere else
(defn make [^clojure.lang.Keyword k ^double splitx ^long nleft ^long n]
  (assert (< nleft n))
  ;; make sure NaN goes down the majority vote branch
  (if (> nleft (- n nleft))
    (left/make k splitx)
    (right/make k splitx)))
;;------------------------------------------------------------------------------
