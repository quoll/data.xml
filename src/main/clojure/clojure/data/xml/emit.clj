;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions to parse XML into lazy sequences and lazy trees and
  emit these as text."
  :author "Paul Gearon"}
  clojure.data.xml.emit
  (:require [clojure.string :as str]
            [clojure.data.xml.namespaces :as ns])
  (:import [clojure.data.xml.event Event]
           [clojure.data.xml.node Element CData Comment]))

(defprotocol EventGeneration
  "Protocol for generating new events based on element type"
  (gen-event [item]
    "Function to generate an event for e.")
  (next-events [item next-items]
    "Returns the next set of events that should occur after e.  next-events are the
     events that should be generated after this one is complete."))

(extend-protocol EventGeneration
  Element
  (gen-event [element]
    (Event. :start-element (:tag element) (:attrs element) (:namespaces element) nil))
  (next-events [element next-items]
    (cons (:content element)
          (cons (Event. :end-element (:tag element) nil nil nil) next-items)))
  Event
  (gen-event [event] event)
  (next-events [_ next-items]
    next-items)

  clojure.lang.Sequential
  (gen-event [coll]
    (gen-event (first coll)))
  (next-events [coll next-items]
    (if-let [r (seq (rest coll))]
      (cons (next-events (first coll) r) next-items)
      (next-events (first coll) next-items)))
  
  String
  (gen-event [s]
    (Event. :chars nil nil nil s))
  (next-events [_ next-items]
    next-items)

  Boolean
  (gen-event [b]
    (Event. :chars nil nil nil (str b)))
  (next-events [_ next-items]
    next-items)

  Number
  (gen-event [b]
    (Event. :chars nil nil nil (str b)))
  (next-events [_ next-items]
    next-items)

  CData
  (gen-event [cdata]
    (Event. :cdata nil nil nil (:content cdata)))
  (next-events [_ next-items]
    next-items)
  
  Comment
  (gen-event [comment]
    (Event. :comment nil nil nil (:content comment)))
  (next-events [_ next-items]
    next-items)
  
  nil
  (gen-event [_]
    (Event. :chars nil nil nil ""))
  (next-events [_ next-items]
    next-items))


(defn qualified-name [event-name]
  (if (instance? clojure.lang.Named event-name)
    (let [ns (namespace event-name)]
     [ns (name event-name)])
   (let [name-parts (str/split event-name #"/" 2)]
     (if (= 2 (count name-parts))
       name-parts
       [nil (first name-parts)]))))

(defn write-attributes [attrs ^javax.xml.stream.XMLStreamWriter writer stack]
  (doseq [[k v] attrs]
    (let [[attr-ns attr-name _] (qualified-name k)]
      (if attr-ns
        (.writeAttribute writer attr-ns (ns/stack-lookup stack attr-ns "") attr-name (str v))
        (.writeAttribute writer attr-name (str v))))))

(defn emit-start-tag [event ^javax.xml.stream.XMLStreamWriter writer stack]
  (let [{{default-namespace :xmlns :as namespaces} :namespaces nm :name} event
        [prefix qname] (qualified-name nm)
        p (and prefix (keyword prefix))
        nspace (ns/lookup-prefix stack namespaces prefix default-namespace)]
    (.writeStartElement writer (or prefix "") qname (or nspace ""))
    (when-not (or (str/blank? default-namespace)
                  (ns/lookup-prefix stack namespaces prefix))
      (.writeDefaultNamespace writer default-namespace))
    (doseq [[pre uri] namespaces :when (not (#{:xmlns} pre))]
      (.writeNamespace writer (name pre) uri))
    (let [new-stack (ns/push-namespace stack namespaces)]
      (write-attributes (:attrs event) writer new-stack)
      new-stack)))

(defn emit-cdata [^String cdata-str ^javax.xml.stream.XMLStreamWriter writer]
  (when-not (str/blank? cdata-str) 
    (let [idx (.indexOf cdata-str "]]>")]
      (if (= idx -1)
        (.writeCData writer cdata-str )
        (do
          (.writeCData writer (subs cdata-str 0 (+ idx 2)))
          (recur (subs cdata-str (+ idx 2)) writer))))))

(defn emit-event [event ^javax.xml.stream.XMLStreamWriter writer stack]
  (case (:type event)
    :start-element (emit-start-tag event writer stack)
    :end-element (do (.writeEndElement writer) (pop stack))
    :chars (do (.writeCharacters writer (:str event)) stack)
    :cdata (do (emit-cdata (:str event) writer) stack)
    :comment (do (.writeComment writer (:str event)) stack)))

(defn flatten-elements [elements]
  (when (seq elements)
    (lazy-seq
     (let [e (first elements)]
       (cons (gen-event e)
             (flatten-elements (next-events e (rest elements))))))))

