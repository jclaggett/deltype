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
(defmulti monoid? "Return true if named method is a monoid" vector)
(defmethod monoid? :default [obj mname] false)
(defmethod monoid? [clojure.lang.Associative 'assoc] [_ _] true)
(defmethod monoid? [clojure.lang.IObj 'withMeta] [_ _] true)
(defmethod monoid? [clojure.lang.IPersistentCollection 'cons] [_ _] true)
(defmethod monoid? [clojure.lang.IPersistentCollection 'empty] [_ _] true)
(defmethod monoid? [clojure.lang.IPersistentMap 'assocEx] [_ _] true)
(defmethod monoid? [clojure.lang.IPersistentMap 'assoc] [_ _] true)
(defmethod monoid? [clojure.lang.IPersistentMap 'without] [_ _] true)
(defmethod monoid? [clojure.lang.ITransientAssociative 'assoc] [_ _] true)
(defmethod monoid? [clojure.lang.ITransientCollection 'conj] [_ _] true)
(defmethod monoid? [clojure.lang.ITransientMap 'assoc] [_ _] true)
(defmethod monoid? [clojure.lang.ITransientMap 'without] [_ _] true)

(defn monoid-preferences [mname & ifaces]
  (reduce (fn [lower-ifaces iface]
            (doseq [lower-iface lower-ifaces]
              (prefer-method monoid? [iface mname]
                             [lower-iface mname]))
            (conj lower-ifaces iface))
          []
          (reverse ifaces)))

(monoid-preferences 'assoc
                    clojure.lang.Associative
                    clojure.lang.ITransientAssociative
                    clojure.lang.IPersistentMap
                    clojure.lang.ITransientMap)

(monoid-preferences 'without
                    clojure.lang.IPersistentMap
                    clojure.lang.ITransientMap)

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
    (symbol (.getName ^Class obj))
    (when (map? obj)
      (.sym ^clojure.lang.Var (:var obj)))))

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
   (for [{:keys [field name iname monoid arglists ns]} method-specs
          arglist arglists]
      (let [args (map gensym arglist)
            call (if ns
                   (symbol ns (str name))
                   (symbol (str "." name)))
            body `(~call ~(with-meta field {:tag iname}) ~@args)
            body (if monoid
                   `(~(symbol (str tname ".")) ~@(replace {field body}
                                                          fields))
                   body)]
        [iname `(~name [_# ~@args] ~body)]))
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
    `(deftype ~tname ~fields ~@opts ~@specs)))
