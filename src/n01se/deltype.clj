(ns n01se.deltype
  "Augmented deftype sporting a new :delegate option.")

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


;; monoid role code
;; This is a multimethod for defining the role of a method for a given interface
;; and method name.
(defmulti monoid?
  "Return true if named method is a monoid"
  (fn [obj mname] obj))

(defmethod monoid? :default
  [obj mname]
  (when (class? obj)
    (println "monoid? multimethod not specified for:"
             obj "( method:" mname ")" ))
  false)

(defmethod monoid? clojure.lang.IPersistentCollection
  [_ mname] (contains? '#{cons empty} mname))

(defmethod monoid? clojure.lang.Seqable
  [_ mname] (contains? '#{} mname))

(defmethod monoid? clojure.lang.IObj
  [_ mname] (contains? '#{withMeta} mname))

(defmethod monoid? clojure.lang.Counted
  [_ mname] (contains? '#{} mname))

(defmethod monoid? clojure.lang.IPersistentMap
  [_ mname] (contains? '#{} mname))

;; Parsing code for the deltype macro

;; use the hidden parsing functions in clojure.core
(def parse-opts (resolve 'clojure.core/parse-opts))
(def parse-impls (resolve 'clojure.core/parse-impls))
(defn parse-method-names [specs]
  (->> specs
    (filter list?)
    (map first)
    set))

(defn interface? [klass]
  (.isInterface klass))

(defn get-ifaces [obj]
  (if (class? obj)
    (if (interface? obj)
      (cons obj (supers obj))
      (filter interface? (supers obj)))
    (when (map? obj) ;; assume it is a protocol
      [obj])))

(defn get-iname [obj]
  (if (class? obj)
    (symbol (.getName obj))
    (when (map? obj)
      (.sym (:var obj)))))

(defn get-sigs [obj]
  (->>
    (if (class? obj)
      (->>
        (.getDeclaredMethods obj)
        (map (juxt #(-> % .getName symbol)
                   #(-> % .getParameterTypes count (take letters) vec)))
        (reduce
          (fn [m [mname arglist]]
            (-> m
              (update-in [mname :arglists] conj arglist)
              (assoc-in [mname :name] mname)))
          {}))
      (->>
        (:sigs obj)
        (map-items (fn [k v]
                     [(symbol (name k))
                      (assoc v :arglists (map #(-> % rest vec rest)
                                              (:arglists v)))]))))

    ;; associate monoid status with methods
    (map-vals #(if (contains? % :monoid)
                 %
                 (assoc % :monoid (monoid? obj (:name %)))))))

(defn lookup [x]
  (let [x (resolve x)]
    (if (var? x)
      (deref x)
      x)))


(defn list-ifaces [iface]
  (->>
    (get-ifaces iface)
    (map (juxt get-iname get-sigs))
    (into {})))

(defn parse-delegates [delegates mnames]
  (->>
    (partition-all 2 delegates)
    ;; expand all delegation 'types' into their interfaces and methods.
    (map (fn [[field iname]]
           [field (map (juxt get-iname get-sigs)
                       (get-ifaces (lookup iname)))]))
    ;; denormalize all needed delegate values into maps.
    (mapcat (fn [[field ifaces]]
              (for [[iname sigs] ifaces
                    [mname {:keys [arglists monoid]}] sigs]
                {:field field, :iname iname, :mname mname,
                 :monoid monoid, :arglists arglists})))
    ;; keep only distinct method delegates.
    (reduce (fn [state v]
              (if (contains? (:mnames state) (:mname v))
                state
                (-> state
                    (update-in [:mnames] conj (:mname v))
                    (update-in [:output] conj v))))
            {:mnames mnames :output []})
    :output))

(defn inject-methods [impls delegates tname fields]
  (->>
    ;; emit delegation methods for all delegate maps
    (for [{:keys [field mname iname monoid arglists]} delegates
          arglist arglists]
      (let [args (map gensym arglist)
            body `(~(symbol (str "." mname)) ~field ~@args)
            body (if monoid
                   `(~(symbol (str tname ".")) ~@(replace {field body}
                                                          fields))
                   body)]
        [iname `(~mname [_# ~@args] ~body)]))
    ;; inject methods into impls
    (reduce (fn [impls [iname method]]
              (update-in impls [iname] conj method))
            impls)))

(defmacro deltype
  "Built on top of clojure's standard deftype and is the same in every way
  except for the new :delegate option.

  The :delegate option takes as a value a vector of pairs of field names to
  class/interface/protocol names. Each field name must also appear in the type's
  overall field vector. field name's can be repeated and so associated with
  multiple class/interface/procotols.

  Each delegate class or interface specified will expand to all methods needed
  to implement the interface(s). When a class is specified, it is used as
  a bundle of all the interfaces that it and its super classes implements.
  Protocols are treated as interfaces.

  Example:
  (deftype+ MyMap [sub-map]
    :delegate [sub-map clojure.lang.PersistentHashMap] ;; act like a map

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
        delegates (parse-delegates (:delegate opts) mnames)
        impls (parse-impls specs)
        impls (inject-methods impls delegates tname fields)
        specs (mapcat #(cons (first %) (second %)) impls)
        opts (mapcat identity (dissoc opts :delegate))]
    `(deftype ~tname ~fields ~@opts ~@specs)))
