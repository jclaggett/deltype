deltype
=======

Experiment with automatic delegations using Clojure's deftype (and reify)

## Usage

```clojure
(require '[n01se.deltype :refer [deltype]])
(deltype MapType [map-field]
  :delegate [map-field clojure.lang.APersistentMap])
```
