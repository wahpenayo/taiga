(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2017-01-18"
      :doc "Bootstrap aggregation." }

    taiga.bagging
  
  (:require [zana.api :as z]))
;;------------------------------------------------------------------------------
;; Pass a function, rather than a data set, to the <code>learn</code> fn inside
;; bagging, because we are using 
(defn- lazy-bootstrap-sampler [^Iterable data]
  (let [prng (z/mersenne-twister-generator)]
    (fn bootstrap-sample ^Iterable []
      (z/sample-with-replacement prng data))))
;;------------------------------------------------------------------------------
(defn bagging [options prng learners]
  (let [data (:data options)
        _(println "training records:" (z/count data))
        nterms (long (:nterms options))
        _ (assert (> nterms 0))
        _(println "nterms:" nterms)
        nthreads (long (:nthreads options 
                                  (.availableProcessors (Runtime/getRuntime))))
        _ (assert (> nthreads 0))
        _(println "nthreads:" nthreads)
        period (min nterms (* 4 nthreads))
        terms (z/nmap
                nthreads
                (fn learn [^clojure.lang.IFn learner
                           ^clojure.lang.IFn sampler
                           ^long i] 
                  (let [term (learner (sampler))]
                    (when (zero? (rem (inc i) period)) (println "bags:" (inc i)))
                    term))
                (z/take nterms learners)
                (z/repeatedly nterms #(lazy-bootstrap-sampler data))
                (range nterms))]
    ((:combine options) terms)))
;;------------------------------------------------------------------------------