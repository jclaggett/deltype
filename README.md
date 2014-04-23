deltype
=======

Experiment with automatic delegations using Clojure's deftype (and reify)

## Usage

```clojure
(require '[n01se.deltype :as d])

(d/deftype MapType [map-field]
  :delegate [map-field n01se.deltype.IMap])

(def foo (MapType. {}))

(assoc foo :a 1) ;=> {:a 1}
(class foo) ;=> MapType
```
