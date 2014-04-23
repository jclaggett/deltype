(ns n01se.deltype
  "Augmented deftype sporting a new :delegate option."
  (:refer-clojure :exclude [deftype]))

(alias 'clj 'clojure.core)

;; Generic Code
(def letters
  (map (comp symbol str)
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn map-items
  "Apply f to all key-value pairs in m. f should take two arguments (a key and
  its associated value) and return a sequence of zero or two values (the key and
  value).  If the returned sequence has zero values, no key/value is inserted
  into the return map."
  [f m]
  (reduce-kv
    (fn [m k v]
      (let [result (f k v)]
        (if (empty? result)
          m
          (assoc m (first result) (second result)))))
    (empty m)
    m))

(defn map-keys
  "Apply f to all keys in m."
  [f m]
  (map-items (fn [k v] [(f k) v]) m))

(defn map-vals
  "Apply f to all values in m."
  [f m]
  (map-items (fn [k v] [k (f v)]) m))

(defn interface? [^Class c]
  (.isInterface c))

(defn get-ifaces [obj]
  (if (class? obj)
    (if (interface? obj)
      [obj]
      (->> (supers obj)
           (filter interface?)
           (reduce
            (fn [ifaces iface]
              (if-let [child (some #(when (isa? % iface) %) ifaces)]
                ifaces
                (conj
                 (remove #(isa? iface %) ifaces)
                 iface)))
            nil)))
    (when (map? obj) ;; assume it is a protocol
      [obj])))

;; Parsing code for the deltype macro

;; use the hidden parsing functions in clojure.core
(def parse-opts (resolve 'clojure.core/parse-opts))
(def parse-impls (resolve 'clojure.core/parse-impls))
(defn parse-method-names [specs]
  (->> specs
    (filter list?)
    (map first)
    set))

(defn get-iname [obj]
  (if (class? obj)
    (symbol (.getName ^Class obj))
    (when (map? obj)
      (.sym ^clojure.lang.Var (:var obj)))))

(declare updater?)

(defn get-sigs [obj]
  (->>
    (if (class? obj)
      (->>
       (.getMethods ^Class obj)
       (map (juxt #(-> ^java.lang.reflect.Method % .getName symbol)
                  #(-> ^java.lang.reflect.Method % .getParameterTypes count (take letters) vec)))
       (reduce
        (fn [m [name arglist]]
          (-> m
              (update-in [name :arglists] (fnil conj #{}) arglist)
              (assoc-in [name :name] name)))
        {}))
      (let [ns-str (-> obj ^clojure.lang.Var (:var) .ns .toString)]
        (->>
          (:sigs obj)
          (map-vals (fn [v]
                      (assoc v
                        :ns ns-str
                        :arglists (map #(-> % rest vec)
                                       (:arglists v))))))))

    ;; associate updater status with methods
    (map-vals #(if (contains? % :updater)
                 %
                 (assoc % :updater (updater? obj (:name %)))))))

(defn get-methods [iface]
  (->>
    (get-ifaces iface)
    (map (juxt get-iname get-sigs))
    (into {})))

(defn lookup [x]
  (let [x (resolve x)]
    (if (var? x)
      (deref x)
      x)))

(defn parse-delegates [delegates]
  (->>
    delegates
    (partition-all 2)
    ;; expand all delegation 'types' into their name and methods.
    (map (fn [[field iname]]
           (let [iface (lookup iname)]
             [field (get-iname iface) (get-sigs iface)])))))

(defn inject-inames [impls inames]
  (merge (zipmap inames (repeat [])) impls))

(defn get-mspecs [delegates mnames]
  (->> delegates
    ;; Reverse so later delegates override earlier delegates.
    reverse
    ;; Denormalize all needed delegate values into maps.
    (mapcat (fn [[field iname msigs]]
              (for [sig (vals msigs)]
                (assoc sig :field field, :iname iname))))
    ;; Reduce method delegates to those not manually specified and so
    ;; only one method spec remains (with possibly multiple inames).
    (reduce (fn [state v]
              (if (or (contains? mnames (:name v))
                      (contains? state (:name v)))
                state
                (assoc state (:name v) v)))
            {})
    vals))

(defn inject-methods [impls method-specs tname fields]
  (->>
    ;; emit delegation methods for all delegate maps
   (for [{:keys [field name iname updater arglists ns]} method-specs
          arglist arglists]
      (let [args (map gensym arglist)
            call (if ns
                   (symbol ns (str name))
                   (symbol (str "." name)))
            body `(~call ~(with-meta field {:tag iname}) ~@args)
            body (if updater
                   `(~(symbol (str tname ".")) ~@(replace {field body}
                                                          fields))
                   body)]
        [iname `(~name [_# ~@args] ~body)]))
    ;; inject methods into impls
    (reduce (fn [impls [iname method]]
              (update-in impls [iname] conj method))
            impls)))

(defmacro deftype
  "Built on top of clojure's standard deftype and is the same in every
  way except for the new :delegate option.

  The :delegate option takes as a value a vector of pairs of field
  names to interface/protocol names. Each field name must also appear
  in the type's overall field vector. field name's can be repeated and
  so associated with multiple interface/procotols.

  Each specified delegate interface will expand to the methods needed
  to implement the interface. These methods are either simple
  delegators or 'updater' delgators. The updater? multimethod controls
  which type of delegator should be used.

  Example:
  (deftype MyMap [sub-map]
    :delegate [sub-map clojure.lang.IPersistentMap]

    clojure.lang.IPersistentMap
    ;; manually define just the assoc behavior
    (assoc [_ k v]
      (println \"Associng a new key into map: \" k)
      (MyMap. (.assoc sub-map k v))))

  ;; MyMap acts just like a map.
  (-> (MyMap. {:a 1})
    (assoc :b 2))"

  [tname fields & opts+specs]
  (let [[opts specs] (parse-opts opts+specs)
        mnames (parse-method-names specs)
        delegates (parse-delegates (:delegate opts))
        impls (parse-impls specs)
        impls (inject-inames impls (map second delegates))
        impls (inject-methods impls
                              (get-mspecs delegates mnames)
                              tname fields)
        specs (mapcat #(cons (first %) (second %)) impls)
        opts (mapcat identity (dissoc opts :delegate))]
    `(clj/deftype ~tname ~fields ~@opts ~@specs)))

;; Updater multi-method
;; This is a multimethod for specifying if a method on a class is an
;; 'updater'. Specifically, any method that returns a new instance of
;; the class it is attached to is probably an 'updater'.
(defmulti updater? "Return true if named method returns a new copy of 'this'." vector)
(defmethod updater? :default [obj mname] false)

;; Updater? definitions for Clojure's builtin collections.
(defmethod updater? [clojure.lang.Associative 'assoc] [_ _] true)
(defmethod updater? [clojure.lang.IObj 'withMeta] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentCollection 'cons] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentCollection 'empty] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentMap 'assocEx] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentMap 'assoc] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentMap 'without] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentList 'pop] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentSet 'disjoin] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentVector 'pop] [_ _] true)
(defmethod updater? [clojure.lang.IPersistentVector 'assocN] [_ _] true)
(defmethod updater? [clojure.lang.ITransientAssociative 'assoc] [_ _] true)
(defmethod updater? [clojure.lang.ITransientCollection 'conj] [_ _] true)
(defmethod updater? [clojure.lang.ITransientMap 'assoc] [_ _] true)
(defmethod updater? [clojure.lang.ITransientMap 'without] [_ _] true)
(defmethod updater? [clojure.lang.ITransientSet 'conj] [_ _] true)
(defmethod updater? [clojure.lang.ITransientSet 'disjoin] [_ _] true)
(defmethod updater? [clojure.lang.ITransientVector 'pop] [_ _] true)
(defmethod updater? [clojure.lang.ITransientVector 'assocN] [_ _] true)

(defn updater-rank [mname & ifaces]
  (->> ifaces
       reverse
       (reduce (fn [lower-ifaces iface]
                 (doseq [lower-iface lower-ifaces]
                   (prefer-method updater? [iface mname]
                                  [lower-iface mname]))
                 (conj lower-ifaces iface))
               [])))

(updater-rank 'assoc
              clojure.lang.Associative
              clojure.lang.ITransientAssociative
              clojure.lang.IPersistentMap
              clojure.lang.ITransientMap)

(updater-rank 'without
              clojure.lang.IPersistentMap
              clojure.lang.ITransientMap)

(updater-rank 'disjoin
              clojure.lang.IPersistentSet
              clojure.lang.ITransientSet)

(updater-rank 'pop
              clojure.lang.IPersistentList
              clojure.lang.IPersistentVector
              clojure.lang.ITransientVector)

(updater-rank 'assocN
              clojure.lang.IPersistentVector
              clojure.lang.ITransientVector)

;; Convenience interfaces that combine the standard interfaces into a
;; single interface to be used to mimic standard datatypes.
(defmacro defextender
  "Macro similar to definterface except that this one supports
  extending (and only extending) other interfaces."
  [name & ifaces]
  (let [cname (with-meta (symbol (str (namespace-munge *ns*) "." name)) (meta name))]
    `(let []
       (gen-interface :name ~cname :extends [~@(map resolve ifaces)])
       (import ~cname))))

;; Clojure Map related interfaces

(defextender IMap
  clojure.lang.IFn
  clojure.lang.IHashEq
  clojure.lang.IObj
  clojure.lang.IPersistentMap
  clojure.lang.MapEquivalence
  java.io.Serializable
  java.lang.Iterable
  java.util.Map)

(defextender IEditableMap
  IMap
  clojure.lang.IEditableCollection
  clojure.lang.ITransientMap)

;; Since IEditableMap merges persistent and transient methods into a
;; single interface, the methods that convert from one to the other
;; are now updaters.
(defmethod updater? [IEditableMap 'asTransient] [_ _] true)
(defmethod updater? [IEditableMap 'persistent] [_ _] true)

;; Clojure Set related interfaces

(defextender ISet
  clojure.lang.IFn
  clojure.lang.IHashEq
  clojure.lang.IObj
  clojure.lang.IPersistentSet
  java.io.Serializable
  java.util.Set)

(defextender IEditableSet
  ISet
  clojure.lang.IEditableCollection
  clojure.lang.ITransientSet)

(defmethod updater? [IEditableSet 'asTransient] [_ _] true)
(defmethod updater? [IEditableSet 'persistent] [_ _] true)

;; Clojure List interface

(defextender IList
  clojure.lang.Counted
  clojure.lang.IHashEq
  clojure.lang.IObj
  clojure.lang.IPersistentList
  clojure.lang.IReduce
  clojure.lang.ISeq
  java.io.Serializable
  ;; I need to handle (.remove _ ^int n) and (.remove _ ^Object o)
  ;; java.util.List
  )

;; Clojure Vector interfaces

(defextender IVector
  clojure.lang.IFn
  clojure.lang.IHashEq
  clojure.lang.IObj
  clojure.lang.IPersistentVector
  java.io.Serializable
  java.lang.Comparable
  ;; I need to handle (.remove _ ^int n) and (.remove _ ^Object o)
  ;; java.util.List
  java.util.RandomAccess)

(defextender IEditableVector
  IVector
  clojure.lang.IEditableCollection
  clojure.lang.ITransientVector)

(defmethod updater? [IEditableVector 'asTransient] [_ _] true)
(defmethod updater? [IEditableVector 'persistent] [_ _] true)
