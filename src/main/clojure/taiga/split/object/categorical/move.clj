(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(ns ^{:author ["wahpenayo at gmail dot com"
               "John Alan McDonald" 
               "Kristina Lisa Klinkner"]
      :since "2016-11-29"
      :date "2017-11-06"
      :doc "Generate a sequence of moves used to search all 
            binary partitions of a set of categories, such that 
            the number of moves decreases as the order of the 
            categories in the passed list. The idea is that a move
            of a category has a computation cost proportional to 
            the number of cases in that category." }
    
    taiga.split.object.categorical.move
  
  (:refer-clojure :exclude [reverse]))
;;----------------------------------------------------------------
(deftype Move [^boolean right? ^Object item]

  Object
  (equals [this that]
    (and (instance? Move that)
         (= right? (.right? ^Move that))
         (= item (.item ^Move that))))
  (hashCode [this]
    (let [result (unchecked-multiply-int (int 31) (int 17))
          result (if right? 
                   (unchecked-add-int (int 1) result) 
                   result)]
       (+ (.hashCode item) 
          (unchecked-multiply-int (int 31) result))))
  (toString [this]
    (str (if right? "[right " "[left ") (.toString item) "]")))

(defmethod print-method Move [^Move this ^java.io.Writer w] 
  (.write w (.toString this)))
;;----------------------------------------------------------------
(defn -move- [thing right?] (Move. (boolean right?) thing))
(def move (memoize -move-))
(defn item [^Move m] (.item m))
(defn right? [^Move m] (.right? m))
(defn reverse [m] (move (item m) (not (right? m))))
;;----------------------------------------------------------------
(defn moves 
  "Return a list of moves that take us thru all binary partitions 
   of a sorted set, where the earlier items in the set move more 
   often than the later ones."
  [things]  
  (if (empty? things)
    []
    (let [tmp (moves (butlast things))
          mvs (conj tmp (move (last things) true))]
      (loop [tmp (rseq tmp)
             mvs mvs]      
        (if (empty? tmp)
          mvs
          (recur (rest tmp) (conj mvs (reverse (first tmp)))))))))
;;----------------------------------------------------------------
