(set! *warn-on-reflection* true)
(set! *unchecked-math* false)
(ns ^{:author ["John Alan McDonald" "Kristina Lisa Klinkner"] 
      :date "2018-08-17"
      :doc "Greedy decision tree splitting." }
    
    taiga.tree.split.numericalleft
  
  (:require [clojure.string :as s]
            [cheshire.generate]
            [zana.api :as z]
            [taiga.utils :as utils]
            [taiga.tree.node :as node]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
;; NaN (missing) will go to the true child

(deftype NumericalSplitDefaultLeft [^clojure.lang.Keyword k 
                                    ^double splitx
                                    ^taiga.tree.node.Node true-child
                                    ^taiga.tree.node.Node false-child]
  taiga.tree.node.Node
  (isLeaf [this] false)
  (trueChild [this] true-child)
  (falseChild [this] false-child)
  (child [this predictors datum]
    (let [^java.util.Map p predictors
          ^clojure.lang.IFn$OD x (.get p k)
          _ (assert x (print-str 
                        "predictor" k "not found in\n" 
                        (str (into {} 
                                   (map (fn [[k v]] [k (z/name v)]) 
                                        p)))))
          xi (x datum)
          ;; (not (<= ...)) to handle NaN
          direction (not (<= (.invokePrim x datum) splitx))]
      
      (when utils/*debug*
        (println "-------------------------")
        (println (.getSimpleName (class this)))
        (println "on" k "->" (z/name x) "->" xi)
        (println "(not (<=" xi splitx "))")
        (println "branch:" direction)
        (println))
      
      (if direction true-child false-child)))
  
  (^clojure.lang.IFn extract [this ^java.util.Map predictors]
    (let [^clojure.lang.IFn$OD x (.get predictors k)]
      (assert x (print-str "predictor" k "not found in\n" 
                           (str (into {} (map (fn [[k v]] [k (z/name v)]) 
                                              predictors)))))
      (fn left-split-predicate [datum] 
        ;; (not (<= ...)) to handle NaN
        (not (<= (.invokePrim x datum) splitx)))))
  
  (withChildren [this child0 child1]
    (NumericalSplitDefaultLeft. k splitx child0 child1))
  (withChildren [this children] 
    (.withChildren this (first children) (second children)))
  
  (isFertile [this] true)
  (getChildren [this] (seq [true-child false-child]))
  
  clojure.lang.IFn$OOD
  (invokePrim [this predictors datum] 
    (let [^clojure.lang.IFn$OOD leaf (node/leaf this predictors datum)] 
      (.invokePrim leaf predictors datum)))
  
  clojure.lang.IFn
  (invoke [this predictors datum] 
    (let [^clojure.lang.IFn leaf (node/leaf this predictors datum)] 
      (.invoke leaf  predictors datum)))
  
  Object 
  (equals [this that]
    (and (instance? NumericalSplitDefaultLeft that)
         (let [^NumericalSplitDefaultLeft that that]
           (= k (.k that))
           (== splitx (.splitx that)))))
  (toString [this] (str "(> " k " " splitx ")")))
;;------------------------------------------------------------------------------
(defn make 
  ^taiga.tree.split.numericalleft.NumericalSplitDefaultLeft 
  [^clojure.lang.Keyword k
   ^double splitx]
  (NumericalSplitDefaultLeft. k splitx nil nil))
;;------------------------------------------------------------------------------
(defn map->NumericalSplitDefaultLeft [^java.util.Map m] 
  (NumericalSplitDefaultLeft. 
    (:k m) 
    (double (:splitx m))
    (:true-child m) 
    (:false-child m)))

(defn map<-NumericalSplitDefaultLeft 
  ^java.util.Map [^NumericalSplitDefaultLeft s] 
  {;;:class 'taiga.tree.split.numericalleft.NumericalSplitDefaultLeft 
   :class :left
   :k (.k s)
   :splitx (.splitx s)
   :true-child (.true-child s)
   :false-child (.false-child s)})

(defmethod z/clojurize 
  NumericalSplitDefaultLeft [^NumericalSplitDefaultLeft this]
  (map<-NumericalSplitDefaultLeft this))

(defmethod print-method 
  NumericalSplitDefaultLeft [^NumericalSplitDefaultLeft this 
                             ^java.io.Writer w]
  (if *print-readably*
    (do
      (.write w " #taiga.tree.split.numericalleft.NumericalSplitDefaultLeft {:k ")
      (.write w (.toString (.k this)))
      (.write w " :splitx ")
      (.write w (Double/toString (.splitx this)))
      (.write w " :true-child ")
      (.write w (pr-str (.true-child this)))
      (.write w " :false-child ")
      (.write w (pr-str (.false-child this)))
      (.write w "} "))
    (.write w (print-str (map<-NumericalSplitDefaultLeft this)))))
;;------------------------------------------------------------------------------
;; EDN input (output just works)
;;------------------------------------------------------------------------------
(z/add-edn-readers!
  {'taiga.tree.split.numericalleft.NumericalSplitDefaultLeft 
   map->NumericalSplitDefaultLeft})
;;------------------------------------------------------------------------------
;; JSON output (input not supported)
;;------------------------------------------------------------------------------
(defn- encoder [^NumericalSplitDefaultLeft s json-generator]
  (cheshire.generate/encode-map 
    (map<-NumericalSplitDefaultLeft s) json-generator))
(cheshire.generate/add-encoder NumericalSplitDefaultLeft encoder)
;;------------------------------------------------------------------------------
