;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.node
  "Data types for xml nodes: Element, CData and Comment"
  {:author "Herwig Hochleitner"})

; Parsed data format
;; Represents a node of an XML tree
(defrecord Element [tag attrs namespaces content])
(defrecord CData [content])
(defrecord Comment [content])

