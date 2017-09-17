(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-06-24"
      :doc "Enum data type for taiga unit tests." }

    taiga.test.data.primate
  
  (:require [zana.api :as z]))
;;------------------------------------------------------------------------------
(def primates [:hominin :gorilla :orangutan :gibbon 
               :catarrhin :platyrrhin :tarsier :lemur :loris])
(defn generator [seed] (z/random-element-generator primates seed))
(defn ape? [^String k] (#{:hominin :gorilla :orangutan :gibbon} k))
;;------------------------------------------------------------------------------