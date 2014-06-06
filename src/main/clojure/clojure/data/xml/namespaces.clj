;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions to manage namespaces in a stack, and perform lookups in context."
  :author "Paul Gearon"}
  clojure.data.xml.namespaces
  (:require [clojure.string :as str]))

(def default-xml "http://www.w3.org/XML/1998/namespace")

(def empty-stack [{:xml default-xml}])

(defn clean-namespace [ns]
  (into {} (remove (fn [[k v]] (str/blank? v)) ns)))

(defn push-namespace [stack ns]
  (conj stack (clean-namespace (merge (peek stack) ns))))

(defn stack-lookup
  ([stack prefix default] (or (stack-lookup stack prefix) default))
  ([stack prefix] ((keyword prefix) (peek stack))))

(defn lookup-prefix
  "Looks up a prefix for a namespace URI in the local context, then the namespace stack,
   with a possible default namespace."
  ([stack local-ns prefix] (lookup-prefix stack local-ns prefix nil))
  ([stack local-ns prefix default]
   (or
     (if-not (str/blank? prefix)
       (let [p (keyword prefix)]
         (get local-ns p (stack-lookup stack p))))
     default)))

