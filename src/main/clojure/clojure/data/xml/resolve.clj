;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Helper functions for resolving XML structures to QNames"
  :author "Paul Gearon"}
  clojure.data.xml.resolve
  (:import [clojure.data.xml.node Element]
           [javax.xml.namespace QName]))

(defn resolve-to-qname
  "Resolves a keyword representing a QName into the fully resolved QName,
  with the appropriate namespace."
  [inherited-ns current-ns k]
  (if-let [k-ns (namespace k)]
    (let [kw-ns (keyword k-ns)
          uri (get current-ns kw-ns (get inherited-ns kw-ns))]
      (QName. uri (name k) k-ns))
    (QName. (name k))))

(defn resolve-attrs
  "Resolves an attribute/value into a QName for the attribute paired with the value"
  [inherited-ns current-ns attrs]
  (into {} (map (fn [[k v]] [(resolve-to-qname inherited-ns current-ns k) v]) attrs)))

