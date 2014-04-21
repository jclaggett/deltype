(ns n01se.deltype.examples.transient
  (:require [clojure.test :refer [deftest]]
            [clojure.pprint :refer [pprint]]
            [n01se.deltype :refer [deltype]]))

(use 'clojure.repl)

"Problem: defining a new type that delegates to the transient method
 (declared by IEditableCollection) to a sub-field will cause the new
 type to be 'lost' when transient returns a new transient value that
 doesn't know about the top level new type."

(deltype NewMap [map-field]
         :delegate [map-field clojure.lang.IPersistentMap
                    map-field clojure.lang.IEditableCollection])

(-> (NewMap. {:q "initial-value"})
    empty
    (assoc :a 1)
    (doto (-> class prn)) ;=> NewMap
    (into {:b 2 :c 3})
    (doto (-> class prn)) ;=> ArrayMap
    )

"Solution: define a single interface that extends both needed interfaces
 and then explicitly define the interacting methods as monoidal."

;; Note that I can't use definterface since :extends are not supported
;; by it so I must use gen-interface directly.
(gen-interface :name n01se.deltype.examples.transient.INewMap
               :extends [clojure.lang.IPersistentMap
                         clojure.lang.IEditableCollection
                         clojure.lang.ITransientMap
                         clojure.lang.IObj    ;; IObj and IMeta are used
                         clojure.lang.IMeta]) ;; by (into)
(import n01se.deltype.examples.transient.INewMap)

(defmethod n01se.deltype/monoid? [INewMap 'asTransient] [_ _] true)
(defmethod n01se.deltype/monoid? [INewMap  'persistent] [_ _] true)

(deltype NewMap [map-field] :delegate [map-field INewMap])

(-> (NewMap. {:q "initial-value"})
    empty
    (assoc :a 1)
    (doto (-> class prn)) ;=> NewMap
    (into {:b 2 :c 3})
    (doto (-> class prn)) ;=> NewMap
    )
