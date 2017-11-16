(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author ["John Alan McDonald"
               "Kristina Kilnkner"
               "wahpenayo at gmail dot com"]
      :since "2016-11-08"
      :date "2017-11-15"
      :doc "Primary external interface to Taiga, providing a subset of the
            functions found in other taiga namespaces, created using 
            [Potemkin](https://github.com/ztellman/potemkin).
            There are functions here for creating random forests, 
            serializing them to and from files, measuring accuracy and
            attribute importance.
            The basic entry point is [[random-forest]].
            Most users will call [[majority-vote-classifier]],
            [[mean-regression]], or [[positive-fraction-probability]]." }

    taiga.api

  (:require [potemkin.namespaces :as pn]
            [taiga.tree.node :as node]
            [taiga.metrics :as m]
            [taiga.permutation :as p]
            [taiga.ensemble :as e]
            [taiga.forest :as f]))
;;------------------------------------------------------------------------------
;; random forest api
;;------------------------------------------------------------------------------
(pn/import-vars e/parameters
                e/terms
                e/nterms
                e/take-terms
                f/majority-vote-classifier
                f/majority-vote-probability
                #_f/minimum-cost-classifier
                f/mean-regression
                f/mean-regression-options
                f/mean-vector-regression
                f/positive-fraction-probability
                f/pprint-forest
                f/random-forest
                f/read-edn
                f/real-probability-measure-options
                f/real-probability-measure
                f/write-edn
                f/write-json
                node/count-children
                node/count-leaves
                node/node-height
                m/mean-absolute-error
                m/rms-error
                m/true-positives
                m/true-negatives
                m/false-positives
                m/false-negatives
                p/permutation-statistics
                p/statistics-tsv)
;;------------------------------------------------------------------------------
