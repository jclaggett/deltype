(ns notes (:use [n01se.deltype]))
(comment "Notes for the Clojure meetup")

"Jonathan Claggett: Work at ViaSat"

"The problem: hard to make new data types that fully implement the various
interfaces that Clojure's persistent collections implement."
;; show supers result

"The goal: make it easy to define data types that start off acting just like
 maps, sets, vectors or lists."

"The approach: add a new :delegate option to Clojure's deftype macro."
;; show deftype example

"Why is Java's inheritance bad? 1) lose encapsulation 2) Tree hierarchy."
;; http://raganwald.com/2014/03/31/class-hierarchies-dont-do-that.html
;; http://www.javaworld.com/article/2073649/core-java/why-extends-is-evil.html
;; "I'd leave out classes"  -- James Goesling

"Does delegation help with Encapsulation?"
;; yes, sub field may implement however you want.

"Does delegation help with trees?"
;; yes, using interfaces allows for a DAG instead.

"The two types of delegation methods: simple and updaters"

"Questions?"
