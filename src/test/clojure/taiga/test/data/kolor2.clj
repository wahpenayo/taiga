(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; warnings in cheshire.generate
(ns ^{:author "John Alan McDonald" :date "2016-11-07"
      :doc "Enum data type for taiga unit tests." }

    taiga.test.data.kolor2
  
  (:require [cheshire.generate]
            [zana.api :as z]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
(def kolors [:red :yellow :green :cyan :blue :magenta])
(defn generator [seed] (z/random-element-generator kolors seed))
(defn primary? [^String k] (or (= k :red) (= k :green) (= k :blue)))
