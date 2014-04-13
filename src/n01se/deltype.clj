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
  [_ mname] false)

(defmethod monoid? clojure.lang.IObj
  [_ mname] (contains? '#{withMeta} mname))

(defmethod monoid? clojure.lang.Counted
  [_ mname] false)

(defmethod monoid? clojure.lang.ILookup
  [_ mname] false)

(defmethod monoid? clojure.lang.Associative
  [_ mname] (contains? '#{assoc} mname))

(defmethod monoid? clojure.lang.IPersistentMap
  [_ mname] (contains? '#{assoc assocEx without} mname))

(defmethod monoid? java.lang.Iterable
  [_ mname] false)

(defmethod monoid? java.util.concurrent.Callable
  [_ mname] false)

(defmethod monoid? java.util.Map
  [_ mname] false)

(defmethod monoid? clojure.lang.IHashEq
  [_ mname] false)

(defmethod monoid? clojure.lang.IMeta
  [_ mname] false)

(defmethod monoid? clojure.lang.IEditableCollection
  [_ mname] false)

(defmethod monoid? clojure.lang.IFn
  [_ mname] false)

(defmethod monoid? clojure.lang.ITransientMap
  [_ mname] (contains? '#{assoc without} mname))

(defmethod monoid? clojure.lang.ITransientCollection
  [_ mname] (contains? '#{conj} mname))

(defmethod monoid? clojure.lang.ITransientAssociative
  [_ mname] (contains? '#{assoc} mname))

(defmethod monoid? java.lang.Runnable
  [_ mname] false)

;; Parsing code for the deltype macro

;; use the hidden parsing functions in clojure.core
(def parse-opts (resolve 'clojure.core/parse-opts))
(def parse-impls (resolve 'clojure.core/parse-impls))
(defn parse-method-names [specs]
  (->> specs
    (filter list?)
    (map first)
    set))

(defn use-proto [^Class c]
  (let [[_ a b] (re-find #"(.*)[.]([^.]*)$"  (.getName c))
        proto (some-> (symbol a b) resolve deref)]
    (if (= c (:on-interface proto))
      proto
      c)))

(defn interface? [^Class c]
  (.isInterface c))

(defn get-ifaces [obj]
  (if (class? obj)
    (if (interface? obj)
      (->> (supers obj) (cons obj) (map use-proto))
      (->> (supers obj) (filter interface?) (map use-proto)))
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
      (let [ns-str (-> obj :var .ns .toString)]
        (->>
          (:sigs obj)
          (map-items (fn [k v]
                       [(symbol (name k))
                        (assoc v
                               :ns ns-str
                               :arglists (map #(-> % rest vec rest)
                                              (:arglists v)))])))))

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

(defn parse-delegates [delegates]
  (->>
    (partition-all 2 delegates)
    ;; expand all delegation 'types' into their interfaces and methods.
    (map (fn [[field iname]]
           [field (map (juxt get-iname get-sigs)
                       (get-ifaces (lookup iname)))]))))

(defn get-inames
  [delegates]
  (for [[_ ifaces] delegates
        [iname] ifaces]
    iname))

(defn inject-inames [impls inames]
  (merge (zipmap inames (repeat [])) impls))

(defn get-mspecs [delegates mnames]
  (->> delegates
    ;; denormalize all needed delegate values into maps.
    (mapcat (fn [[field ifaces]]
              (for [[iname sigs] ifaces
                    [mname msig] sigs]
                (assoc (select-keys msig [:ns :monoid :arglists])
                       :field field, :iname iname, :mname mname))))
    ;; keep only distinct method delegates.
    (reduce (fn [state v]
              (if (contains? (:mnames state) (:mname v))
                state
                (-> state
                    (update-in [:mnames] conj (:mname v))
                    (update-in [:output] conj v))))
            {:mnames mnames :output []})
    :output))

(defn inject-methods [impls method-specs tname fields]
  (->>
    ;; emit delegation methods for all delegate maps
    (for [{:keys [field mname iname iface monoid arglists ns]} method-specs
          arglist arglists]
      (let [args (map gensym arglist)
            call (if ns
                   (symbol ns (str mname))
                   (symbol (str "." mname)))
            body `(~call ~field ~@args)
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
        delegates (parse-delegates (:delegate opts))
        impls (parse-impls specs)
        impls (inject-inames impls (get-inames delegates))
        impls (inject-methods impls
                              (get-mspecs delegates mnames)
                              tname fields)
        specs (mapcat #(cons (first %) (second %)) impls)
        opts (mapcat identity (dissoc opts :delegate))]
    `(deftype ~tname ~fields ~@opts ~@specs)))
