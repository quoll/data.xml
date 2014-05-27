;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Data structures for representing XML syntax."
  :author "Paul Gearon"}
  clojure.data.xml.syntax
  (:import [clojure.data.xml.node Element CData Comment]))

(defprotocol AsElements
  (as-elements [expr] "Return a seq of elements represented by an expression."))

(defn sexp-element [tag attrs namespaces child]
  (cond
   (= :-cdata tag) (CData. (first child))
   (= :-comment tag) (Comment. (first child))
   :else (Element. tag attrs namespaces (mapcat as-elements child))))

(defn- extract-leading-map
  [[m & remainder :as all]]
  (if (map? m)
    [(into {} (for [[k v] m] [k (str v)])) remainder]
    [{} all]))

(extend-protocol AsElements
  clojure.lang.IPersistentVector
  (as-elements [v]
    (let [[tag & [attrs & after-attrs :as content]] v
          [attrs no-attr-content] (extract-leading-map content)
          [namespaces simple-content] (extract-leading-map no-attr-content)]
      [(sexp-element tag attrs namespaces simple-content)]))

  clojure.lang.ISeq
  (as-elements [s]
    (mapcat as-elements s))

  clojure.lang.Keyword
  (as-elements [k]
    [(Element. k {} {} ())])

  java.lang.String
  (as-elements [s]
    [s])

  nil
  (as-elements [_] nil)

  java.lang.Object
  (as-elements [o]
    [(str o)]))

