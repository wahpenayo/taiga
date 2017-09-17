(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author "John Alan McDonald" :date "2016-12-19" }
    
    taiga.scripts.debug.moves
  
  (:require [clojure.pprint :as pp]
            [taiga.split.object.categorical.move :as move]))
;;------------------------------------------------------------------------------
(let [left #{:a :b :c}
      right #{:d}
      moves (move/moves (sort left))]
  (loop [moves moves
         left left
         right right]
    (println (sort left) (sort right))
    (when-not (empty? moves)
      (let [move (first moves)
            item (move/item move)
            right? (move/right? move)]
        (println move)
        (if right?
          (recur (rest moves) (disj left item) (conj right item))
          (recur (rest moves) (conj left item) (disj right item)))))))
;;------------------------------------------------------------------------------
