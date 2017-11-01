(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; warnings in cheshire.generate
(ns ^{:author "wahpenayo at gmail dot com" 
      :date "2017-10-31"
      :doc "Enum data type for taiga unit tests." }

    taiga.test.measure.data.kolor
  
  (:require [cheshire.generate]
            [zana.api :as z])
  (:import [java.util EnumSet]
           [taiga.test.java.data Kolor]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
(def kolors [Kolor/RED Kolor/YELLOW Kolor/GREEN 
             Kolor/CYAN Kolor/BLUE Kolor/MAGENTA])
(defn generator [seed] (z/random-element-generator kolors seed))
(def ^EnumSet primaries
  (EnumSet/of Kolor/RED Kolor/GREEN Kolor/BLUE))
(defn primary? [^Kolor k] (.contains primaries k))
;;------------------------------------------------------------------------------
(defmethod z/clojurize Kolor [^Kolor this] (.name this))
(defmethod print-method 
  taiga.test.java.data.Kolor 
  [^Kolor this ^java.io.Writer w]
  (.write w (.toString this)))
;;------------------------------------------------------------------------------
(z/add-edn-readers! 
  {'taiga.test.java.data.Kolor #(Kolor/valueOf taiga.test.java.data.Kolor %)})
;;------------------------------------------------------------------------------
(cheshire.generate/add-encoder 
  taiga.test.java.data.Kolor
  (fn encode-Kolor [^taiga.test.java.data.Kolor c 
                    ^com.fasterxml.jackson.core.JsonGenerator jsonGenerator]
    (.writeString jsonGenerator (.toString c))))
;;------------------------------------------------------------------------------
