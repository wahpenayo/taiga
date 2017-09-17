;; codox has reflection and boxed math warnings
(ns ^{:author "John Alan McDonald" :date "2016-12-19"}
    
    taiga.scripts.doc.codox
  
  (:require [clojure.java.io :as io]
            [codox.main :as codox])
  (:import [org.eclipse.jgit.storage.file FileRepositoryBuilder]))
;;------------------------------------------------------------------------------
(let [branch (.getBranch 
               (.build 
                 (.findGitDir
                   (.readEnvironment
                     (.setGitDir 
                       (FileRepositoryBuilder.)
                       (io/file ".git"))))))]
  (codox/generate-docs
    {:name "Taiga" 
     :version "4.0.0" 
     :description "A random forest library."
     :root-path "."
     :source-paths ["src/main/clojure"]
     :namespaces [#"\.api$"]
     ;;:html {:namespace-list :flat}
     :metadata {:doc "--needs documentation--" :doc/format :markdown}
     :doc-paths ["src/doc/codox"]
     :output-path "target/codox"
     :source-uri (str ""
                      branch
                      "/--/src/main/clojure/{classpath}#L{line}")}))
;;:source-uri "file:///{filepath}#line={line}"
;;------------------------------------------------------------------------------

