(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "wahpenayo at gmail dot com"
      :date "2017-10-31"
      :doc "Enum data type for taiga unit tests." }

    taiga.test.regress.data.primate
  
  (:require [zana.api :as z]))
;;------------------------------------------------------------------------------
(def primates [:hominin :gorilla :orangutan :gibbon 
               :catarrhin :platyrrhin :tarsier :lemur :loris])
(defn generator [seed] (z/random-element-generator primates seed))
(defn ape? [^String k] (#{:hominin :gorilla :orangutan :gibbon} k))
;;------------------------------------------------------------------------------