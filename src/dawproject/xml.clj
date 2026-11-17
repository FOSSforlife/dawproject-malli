(ns dawproject.xml
  "Reading and writing DAWproject XML / .dawproject (zip) files in the data
  model described by `dawproject.schema`."
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [dawproject.schema :as ds]
            [malli.core :as m])
  (:import (java.util.zip ZipEntry ZipFile ZipOutputStream)))

(defn- element? [x] (and (map? x) (contains? x :tag)))

(defn- ->plain
  "Converts data.xml Element records into plain maps with vector content,
  dropping insignificant whitespace between child elements, and removing
  empty :attrs / :content."
  [node]
  (if (element? node)
    (let [children (:content node)
          children (if (some element? children)
                     (remove #(and (string? %) (str/blank? %)) children)
                     children)
          children (mapv ->plain children)
          attrs (into {} (:attrs node))]
      (cond-> {:tag (:tag node)}
        (seq attrs) (assoc :attrs attrs)
        (seq children) (assoc :content children)))
    node))

(defn parse-raw
  "Parses XML (string, file, stream, ...) into the untyped data model:
  attribute values remain strings."
  [source]
  (->plain
   (if (string? source)
     (xml/parse-str source)
     (with-open [in (io/input-stream source)]
       (xml/parse in)))))

(defn decode
  "Coerces raw attribute strings into typed values according to `schema`
  (default: the DAWproject `Project` schema)."
  ([raw] (decode ds/Project raw))
  ([schema raw] (m/decode schema raw ds/options ds/xml-transformer)))

(defn encode
  "Turns typed values back into XML attribute strings."
  ([doc] (encode ds/Project doc))
  ([schema doc]
   (walk/postwalk
    (fn [x]
      (if (and (map? x) (contains? x :tag) (map? (:attrs x)))
        (update x :attrs update-vals str)
        x))
    (m/encode schema doc ds/options ds/xml-transformer))))

(defn parse-project
  "Parses project.xml content into the typed data model."
  [source]
  (decode ds/Project (parse-raw source)))

(defn parse-metadata
  "Parses metadata.xml content into the typed data model."
  [source]
  (decode ds/MetaData (parse-raw source)))

(defn- ->xml-element [node]
  (if (element? node)
    (xml/element (:tag node) (:attrs node {}) (map ->xml-element (:content node)))
    node))

(defn emit-str
  "Serializes a typed document (Project or MetaData) to an XML string."
  ([doc] (emit-str (if (= :MetaData (:tag doc)) ds/MetaData ds/Project) doc))
  ([schema doc]
   (xml/indent-str (->xml-element (encode schema doc)) :encoding "UTF-8")))

;; ---------------------------------------------------------------------------
;; .dawproject container (zip)
;; ---------------------------------------------------------------------------

(defn read-dawproject
  "Reads a .dawproject zip. Returns {:project <typed Project>,
  :metadata <typed MetaData or nil>, :entries [names of all zip entries]}."
  [file]
  (with-open [zf (ZipFile. (io/file file))]
    (let [read-entry (fn [name]
                       (when-let [e (.getEntry zf name)]
                         (slurp (.getInputStream zf e) :encoding "UTF-8")))
          project (read-entry "project.xml")]
      (when-not project
        (throw (ex-info "Not a DAWproject file: project.xml missing" {:file file})))
      {:project (parse-project project)
       :metadata (some-> (read-entry "metadata.xml") parse-metadata)
       :entries (mapv #(.getName ^ZipEntry %) (enumeration-seq (.entries zf)))})))

(defn write-dawproject
  "Writes a .dawproject zip containing project.xml, metadata.xml and any
  embedded files given as a map of {zip-path source} where source is anything
  `io/input-stream` accepts (File, byte array, URL, ...)."
  ([file project] (write-dawproject file project nil nil))
  ([file project metadata] (write-dawproject file project metadata nil))
  ([file project metadata embedded]
   (with-open [out (ZipOutputStream. (io/output-stream (io/file file)))]
     (let [put! (fn [name ^bytes bs]
                  (.putNextEntry out (ZipEntry. ^String name))
                  (.write out bs)
                  (.closeEntry out))]
       (put! "project.xml" (.getBytes ^String (emit-str ds/Project project) "UTF-8"))
       (put! "metadata.xml" (.getBytes ^String (emit-str ds/MetaData
                                                         (or metadata {:tag :MetaData}))
                                       "UTF-8"))
       (doseq [[path src] embedded]
         (.putNextEntry out (ZipEntry. ^String path))
         (with-open [in (io/input-stream src)] (io/copy in out))
         (.closeEntry out))))
   file))
