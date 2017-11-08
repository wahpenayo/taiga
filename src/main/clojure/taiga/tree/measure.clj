(set! *warn-on-reflection* true)
(set! *unchecked-math* false) ;; warnings in cheshire.generate
(ns ^{:author "wahpenayo at gmail dot com" 
      :since "2017-10-30"
      :date "2017-11-07"
      :doc "probability measure prediction." }
    
    taiga.tree.measure
  
  (:require [clojure.zip :as zip]
            [zana.api :as z]
            [taiga.tree.node :as node]
            [taiga.tree.leaf.double :as dleaf]
            [taiga.tree.leaf.object :as oleaf])
  
  (:import [java.util HashMap Map]
           [clojure.lang IFn IFn$OD]
           [taiga.tree.node Node]
           [taiga.tree.leaf.double Leaf]))
(set! *unchecked-math* :warn-on-boxed)
;;------------------------------------------------------------------------------
;; clojure.zipper interface
;;------------------------------------------------------------------------------
(defn- zipper [^Node root]
  (assert (instance? Node root) (class root))
  (zip/zipper node/fertile? node/children node/with-children 
              root))
;;----------------------------------------------------------------
(defn- replace-leaves [^Map leaf-to-measure loc]
  (if (zip/end? loc) ;; done 
    (zip/root loc) 
    (let [^Node node (zip/node loc)
          loc (if (instance? Leaf node) 
                (zip/replace 
                  loc 
                  (oleaf/make (.get leaf-to-measure node)))
                loc)]
      (recur leaf-to-measure (zip/next loc)))))
;;----------------------------------------------------------------
(defn train
  
  "Replace the leaves in a mean regression tree by ones whose
   values are empirical probability measures.
   <p>
   The returned probability measure will be estimated from the
   <code>training-data</code> that ends up in the same leaf as the
   later <code>datum</code> argument.<br>
   <b>NOTE:</b> if a leaf gets no empirical measure training 
   data, the tree will return <code>nil</code>."
  
  ^Node [^Node root 
         ^IFn$OD ground-truth
         ^Map predictors 
         ^Iterable training-data]
  (let [leaf-ground-truth (node/leaf-ground-truth 
                            root ground-truth predictors 
                            training-data)
        leaf-to-measure (HashMap.)]
    (z/mapc (fn collect-measures [^Node l]
              (let [^doubles ldata (.get leaf-ground-truth l)]
                (.put leaf-to-measure l (z/make-wepdf ldata))))
            (.keySet leaf-ground-truth))
    (replace-leaves leaf-to-measure (zipper root))))
;;----------------------------------------------------------------
