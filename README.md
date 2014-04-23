deltype
=======

An experiment in automatic delegations and Clojure's deftype.

The goal is to make it easy to define new data types that are based off of Clojure's standard persistent collections (i.e., maps, sets, lists, and vectors). The challenge is to do it without using traditional inheritence. The approach is to delegate method calls to a specific field of the new type.

## Usage

```clojure
(require '[n01se.deltype :as d])

(d/deftype MapType [map-field]
  :delegate [map-field n01se.deltype.IMap])

(def foo (MapType. {}))

(assoc foo :a 1) ;=> {:a 1}
(class foo) ;=> MapType
```

## Links
* http://raganwald.com/2014/03/31/class-hierarchies-dont-do-that.html
* http://www.javaworld.com/article/2073649/core-java/why-extends-is-evil.html
