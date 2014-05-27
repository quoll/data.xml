;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions to parse XML into lazy sequences."
  :author "Paul Gearon"}
  clojure.data.xml.parse
  (:require [clojure.string :as str]
            [clojure.data.xml.namespaces :as ns]
            [clojure.data.xml.event :as e]
            [clojure.data.xml.node :as node])
  (:import [javax.xml.stream XMLStreamReader
                             XMLStreamConstants]
           [java.nio.charset Charset]
           [clojure.data.xml.event Event]
           [clojure.data.xml.node Element]))

(defn- attr-prefix [^XMLStreamReader sreader index]
  (let [p (.getAttributePrefix sreader index)]
    (when-not (str/blank? p) p)))

(defn- attr-hash
  [^XMLStreamReader sreader]
  (into {}
    (for [i (range (.getAttributeCount sreader))]
      [(keyword (attr-prefix sreader i) (.getAttributeLocalName sreader i))
       (.getAttributeValue sreader i)])))

(defn- namespaces [^XMLStreamReader sreader stack]
  (into {}
        (keep
          (fn [i]
            (let [prefix (.getNamespacePrefix sreader i)
                  uri (.getNamespaceURI sreader i)]
              (if prefix
                [(keyword prefix) uri]
                (if (not= uri (ns/stack-lookup stack :xmlns))
                  [:xmlns uri]))))
          (range (.getNamespaceCount sreader)))))

(defn- element-keyword [^XMLStreamReader sreader]
  (let [prefix (.getPrefix sreader)]
    (keyword (when-not (str/blank? prefix) prefix) (.getLocalName sreader))))

; Note, sreader is mutable and mutated here in pull-seq, but it's
; protected by a lazy-seq so it's thread-safe.
(defn- pull-seq
  "Creates a seq of events.  The XMLStreamConstants/SPACE clause below doesn't seem to 
   be triggered by the JDK StAX parser, but is by others.  Leaving in to be more complete."
  [^XMLStreamReader sreader stack]
  (lazy-seq
    (loop []
      (condp == (.next sreader)
        XMLStreamConstants/START_ELEMENT
        (let [element-ns (namespaces sreader stack)
              ev (e/event :start-element
                        (element-keyword sreader)
                        (attr-hash sreader)
                        (ns/clean-namespace element-ns)
                        nil)
              loaded-stack (ns/push-namespace stack element-ns)
              current-namespaces (peek loaded-stack)
              [remaining final-stack] (pull-seq sreader loaded-stack)]
          [(cons (with-meta ev current-namespaces) remaining) final-stack])
        XMLStreamConstants/END_ELEMENT
        (let [unwound-stack (pop stack)
              element-name (element-keyword sreader)
              [remaining final-stack] (pull-seq sreader unwound-stack)]
          [(cons (e/event :end-element element-name nil nil nil)
                 remaining)
           final-stack])
        XMLStreamConstants/CHARACTERS
        (if-let [text (and (not (.isWhiteSpace sreader))
                           (.getText sreader))]
          (let [[remaining final-stack] (pull-seq sreader stack)]
            [(cons (e/event :characters nil nil nil text) remaining)
             final-stack])
          (recur))
        XMLStreamConstants/END_DOCUMENT
        nil
        (recur);; Consume and ignore comments, spaces, processing instructions etc
        ))))

(def ^{:private true} xml-input-factory-props
  {:allocator javax.xml.stream.XMLInputFactory/ALLOCATOR
   :coalescing javax.xml.stream.XMLInputFactory/IS_COALESCING
   :namespace-aware javax.xml.stream.XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references javax.xml.stream.XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities javax.xml.stream.XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating javax.xml.stream.XMLInputFactory/IS_VALIDATING
   :reporter javax.xml.stream.XMLInputFactory/REPORTER
   :resolver javax.xml.stream.XMLInputFactory/RESOLVER
   :support-dtd javax.xml.stream.XMLInputFactory/SUPPORT_DTD})

(defn- new-xml-input-factory [props]
  (let [fac (javax.xml.stream.XMLInputFactory/newInstance)]
    (doseq [[k v] props
            :let [prop (xml-input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

(defn source-seq
  "Parses the XML InputSource source using a pull-parser. Returns
   a lazy sequence of Event records.  Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [s & {:as props}]
  (let [fac (new-xml-input-factory (merge {:coalescing true} props))
        ;; Reflection on following line cannot be eliminated via a
        ;; type hint, because s is advertised by fn parse to be an
        ;; InputStream or Reader, and there are different
        ;; createXMLStreamReader signatures for each of those types.
        sreader (.createXMLStreamReader fac s)]
    (first (pull-seq sreader ns/empty-stack))))

(defn seq-tree
  "Takes a seq of events that logically represents
  a tree by each event being one of: enter-sub-tree event,
  exit-sub-tree event, or node event.

  Returns a lazy sequence whose first element is a sequence of
  sub-trees and whose remaining elements are events that are not
  siblings or descendants of the initial event.

  The given exit? function must return true for any exit-sub-tree
  event.  parent must be a function of two arguments: the first is an
  event, the second a sequence of nodes or subtrees that are children
  of the event.  parent must return nil or false if the event is not
  an enter-sub-tree event.  Any other return value will become
  a sub-tree of the output tree and should normally contain in some
  way the children passed as the second arg.  The node function is
  called with a single event arg on every event that is neither parent
  nor exit, and its return value will become a node of the output tree.

  (seq-tree #(when (= %1 :<) (vector %2)) #{:>} str
            [1 2 :< 3 :< 4 :> :> 5 :> 6])
  ;=> ((\"1\" \"2\" [(\"3\" [(\"4\")])] \"5\") 6)"
 [parent exit? node coll]
  (lazy-seq
    (when-let [[event] (seq coll)]
      (let [more (rest coll)]
        (if (exit? event)
          (cons nil more)
          (let [tree (seq-tree parent exit? node more)]
            (if-let [p (parent event (lazy-seq (first tree)))]
              (let [subtree (seq-tree parent exit? node (lazy-seq (rest tree)))]
                (cons (cons p (lazy-seq (first subtree)))
                      (lazy-seq (rest subtree))))
              (cons (cons (node event) (lazy-seq (first tree)))
                    (lazy-seq (rest tree))))))))))

(defn event-tree
  "Returns a lazy tree of Element objects for the given seq of Event
  objects. See source-seq and parse."
  [events]
  (ffirst
   (seq-tree
    (fn [^Event event contents]
      (when (= :start-element (.type event))
        (let [m (meta event)]
          (with-meta (Element. (.name event) (.attrs event) (.namespaces event) contents) m))))
    (fn [^Event event] (= :end-element (.type event)))
    (fn [^Event event] (.str event))
    events)))

