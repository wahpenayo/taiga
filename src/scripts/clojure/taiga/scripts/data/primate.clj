(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-11-23"
      :doc "Enum data type for taiga unit tests." }

    taiga.scripts.data.primate
  
  (:require [zana.api :as z]))
;;------------------------------------------------------------------------------
(def primates 
  [;;:sapiens :neanderthal :denisova :heidelbergensis :erectus :naledi 
   ;;:rudolfensis :ardipithecus :australopithecus :sahelanthropus :orrorin 
   :chimpanzee :bonobo :gorilla :orangutan :gibbon :siamang 
   ;;:cercopithecini :papionini ;;:colobinae :langur :oddnosed
   :marmoset ;;:capuchin :douroucoulis :uakaris :howler :spider
   :tarsier :lemur :loris :galago])
(defn generator [seed] (z/random-element-generator primates seed))
(defn hominid? [k] 
  (#{:sapiens :neanderthal :denisova :heidelbergensis :erectus :naledi 
     :rudolfensis :ardipithecus :australopithecus :sahelanthropus :orrorin} 
    k))
(defn ape? [k] 
  (or (hominid? k) 
      (#{:chimpanzee :bonobo :gorilla :orangutan :gibbon :siamang} k)))
;;------------------------------------------------------------------------------