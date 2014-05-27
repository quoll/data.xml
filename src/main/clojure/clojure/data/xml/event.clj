;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Datatype for items that form an XML stream"
  :author "Paul Gearon"}
  clojure.data.xml.event)


; Represents a parse event.
; type is one of :start-element, :end-element, or :characters
(defrecord Event [type name attrs namespaces str])

(defn event [type name & [attrs namespaces str]]
  (Event. type name attrs namespaces str))

