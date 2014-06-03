;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions to parse XML into lazy sequences and lazy trees and
  emit these as text."
  :author "Chris Houser"}
  clojure.data.xml
  (:require [clojure.string :as str]
            [clojure.data.xml.parse :as parse]
            [clojure.data.xml.emit :as emit]
            [clojure.data.xml.namespaces :as ns]
            [clojure.data.xml.syntax :as syntax]
            [clojure.data.xml.resolve :as res])
  (:import [java.nio.charset Charset]
           [java.io StringReader]
           [javax.xml.namespace QName]
           [clojure.data.xml.event Event]
           [clojure.data.xml.node Element CData Comment]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SEXP structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn element [tag & [attrs & [n & c :as all-c]]]
  (let [[namespaces content] (if (map? n) [n c] [{} all-c])]
    (Element. tag (or attrs {}) namespaces (remove nil? content))))

(defn cdata [content]
  (CData. content))

(defn xml-comment [content]
  (Comment. content))

(defn sexps-as-fragment
  "Convert a compact prxml/hiccup-style data structure into the more formal
   tag/attrs/content format. A seq of elements will be returned, which may
   not be suitable for immediate use as there is no root element. See also
   sexp-as-element.

   The format is [:tag-name attr-map? content*]. Each vector opens a new tag;
   seqs do not open new tags, and are just used for inserting groups of elements
   into the parent tag. A bare keyword not in a vector creates an empty element.

   To provide XML conversion for your own data types, extend the AsElements
   protocol to them."
  ([] nil)
  ([sexp] (syntax/as-elements sexp))
  ([sexp & sexps] (mapcat syntax/as-elements (cons sexp sexps))))

(defn sexp-as-element
  "Convert a single sexp into an Element"
  [sexp]
  (let [[root & more] (sexps-as-fragment sexp)]
    (when more
      (throw
       (IllegalArgumentException.
        "Cannot have multiple root elements; try creating a fragment instead")))
    root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XML Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse
  "Parses the source, which can be an
   InputStream or Reader, and returns a lazy tree of Element records. Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [source & props]
  (parse/event-tree (apply parse/source-seq source props)))

(defn parse-str
  "Parses the passed in string to Clojure data structures.  Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [s & props]
  (let [sr (StringReader. s)]
    (apply parse sr props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XML Emitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-stream-encoding [^java.io.OutputStreamWriter stream xml-encoding]
  (when (not= (Charset/forName xml-encoding) (Charset/forName (.getEncoding stream)))
    (throw (Exception. (str "Output encoding of stream (" xml-encoding
                            ") doesn't match declaration ("
                            (.getEncoding stream) ")")))))

(defn emit
  "Prints the given Element tree as XML text to stream.
   Options:
    :encoding <str>          Character encoding to use"
  [e ^java.io.Writer stream & {:as opts}]
  (let [^javax.xml.stream.XMLStreamWriter writer (-> (javax.xml.stream.XMLOutputFactory/newInstance)
                                                     (.createXMLStreamWriter stream))]

    (when (instance? java.io.OutputStreamWriter stream)
      (check-stream-encoding stream (or (:encoding opts) "UTF-8")))
    
    (.writeStartDocument writer (or (:encoding opts) "UTF-8") "1.0")
    (loop [[event & remaining] (emit/flatten-elements [e]) stack ns/empty-stack]
      (when event
        (recur remaining (emit/emit-event event writer stack))))
    (.writeEndDocument writer)
    stream))

(defn emit-str
  "Emits the Element to String and returns it"
  [e]
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (emit e sw)
    (.toString sw)))

(defn ^javax.xml.transform.Transformer indenting-transformer []
  (doto (-> (javax.xml.transform.TransformerFactory/newInstance) .newTransformer)
    (.setOutputProperty (javax.xml.transform.OutputKeys/INDENT) "yes")
    (.setOutputProperty (javax.xml.transform.OutputKeys/METHOD) "xml")
    (.setOutputProperty "{http://xml.apache.org/xslt}indent-amount" "2")))

(defn indent
  "Emits the XML and indents the result.  WARNING: this is slow
   it will emit the XML and read it in again to indent it.  Intended for 
   debugging/testing only."
  [e ^java.io.Writer stream & {:as opts}]
  (let [sw (java.io.StringWriter.)
        _ (apply emit e sw (apply concat opts))
        source (-> sw .toString StringReader. javax.xml.transform.stream.StreamSource.)
        result (javax.xml.transform.stream.StreamResult. stream)]
    (.transform (indenting-transformer) source result)))

(defn indent-str
  "Emits the XML and indents the result.  Writes the results to a String and returns it"
  [e]
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (indent e sw)
    (.toString sw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XML Emitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Resolvable
  (resolve-xml [r] "Transforms a parsed XML node into a node that has been resolved by namespace"))

(extend-protocol Resolvable
  Element
  (resolve-xml [{:keys [tag attrs namespaces content] :as r}]
    (let [n (meta r)]
      (Element. (res/resolve-to-qname n namespaces tag)
                (res/resolve-attrs n namespaces attrs)
                namespaces
                (map resolve-xml content))))
  Object
  (resolve-xml [r] r))

