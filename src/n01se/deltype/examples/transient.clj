(ns n01se.deltype.examples.transient
  (:require [clojure.test :refer [deftest]]
            [clojure.pprint :refer [pprint]]
            [n01se.deltype :refer [deltype]]))

(use 'clojure.repl)

"Problem: defining a new type that delegates the transient method to a
 subfield will cause the new type to be 'lost' when transient returns
 a new transient value that doesn't know about the top level new
 type."

(deltype NewMap [map-field]
         :delegate [map-field clojure.lang.PersistentHashMap])

(-> (NewMap. {:q "initial-value"})
    empty
    (assoc :a 1)
    (doto (-> class prn)) ;=> NewMap
    (into {:b 2 :c 3})
    (doto (-> class prn)) ;=> ArrayMap
    )

"Solution 1: Unify peristent and transient methods into the new type.
 This means that the new type is maintained through all stages of the
 process."

(deltype NewMap [map-field]
  :delegate [map-field clojure.lang.PersistentHashMap
             map-field clojure.lang.ITransientMap]

  ;; explicitly make these two methods monoidal.
  clojure.lang.IEditableCollection
  (asTransient [_] (NewMap. (.asTransient map-field)))

  clojure.lang.ITransientCollection
  (persistent [_] (NewMap. (.persistent map-field))))

(-> (NewMap. {:q "initial-value"})
    empty
    (assoc :a 1)
    (doto (-> class prn)) ;=> NewMap
    (into {:b 2 :c 3})
    (doto (-> class prn)) ;=> NewMap
    )
