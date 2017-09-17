(ns ^{:author "John Alan McDonald" :date "2016-08-24"}
    
    taiga.scripts.doc.marginalia
  
  (:use marginalia.core [marginalia.html :only (*resources*)]))
;;------------------------------------------------------------------------------
(binding [*resources* ""]
  (ensure-directory! "target/marginalia")
  (uberdoc!
    "target/marginalia/uberdoc.html"
    (format-sources ["src/main/clojure"])
    {:dev-dependencies []
     :name "taiga"
     :description "A random forest kit."
     :version "3.0.0"
     :dependencies [["zana" "1.0.0"]
                    ["marginalia/marginalia" "0.9.0"]]}))
;;------------------------------------------------------------------------------
