(ns dawproject.schema-test
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dawproject.schema :as ds]
            [dawproject.xml :as dx]))

(defn- resource [path] (io/file (io/resource path)))

(defn- reference-files []
  (->> (.listFiles (resource "reference"))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".xml"))
       (sort)))

(defn- kitchen-sink [] (dx/parse-project (resource "kitchen-sink.xml")))

(defn- tags [doc]
  (set (map :tag (tree-seq #(seq (:content %)) #(filter map? (:content %)) doc))))

;; ---------------------------------------------------------------------------
;; Positive cases
;; ---------------------------------------------------------------------------

(deftest reference-implementation-output
  (testing "files written by the Bitwig reference implementation"
    (is (seq (reference-files)))
    (doseq [f (reference-files)
            :let [p (dx/parse-project f)]]
      (testing (.getName ^java.io.File f)
        (is (nil? (ds/humanize p)))
        (is (ds/validate ds/StrictProject p))
        (is (= p (dx/parse-project (dx/emit-str p))) "XML round trip")))))

(deftest kitchen-sink-project
  (let [p (kitchen-sink)]
    (is (nil? (ds/humanize p)))
    (is (= [] (ds/reference-errors p)))
    (is (= p (dx/parse-project (dx/emit-str p))) "XML round trip")
    (testing "fixture covers every global element of Project.xsd"
      (let [registered (disj ds/element-tags :MetaData)]
        (is (= #{} (set/difference registered (tags p))))))))

(deftest metadata
  (let [m (dx/parse-metadata (resource "metadata.xml"))]
    (is (ds/validate ds/MetaData m))
    (is (= ["Kitchen Sink"] (-> m :content first :content)))
    (is (= m (dx/parse-metadata (dx/emit-str m)))))
  (is (ds/validate ds/MetaData {:tag :MetaData}))
  (testing "MetaData children are ordered"
    (is (not (ds/validate ds/MetaData {:tag :MetaData
                                       :content [{:tag :Artist :content ["a"]}
                                                 {:tag :Title :content ["t"]}]})))))

(deftest typed-decoding
  (let [p (kitchen-sink)
        devices (-> p :content (nth 2) :content first :content first :content first :content)
        synth (first devices)
        gate (some #(when (= :NoiseGate (:tag %)) %) devices)
        params (-> synth :content first :content)]
    (is (= {:parameterID 1 :unit :normalized :value 0.25 :min 0.0 :max 1.0 :id "p1"}
           (:attrs (first params))))
    (is (= ["Saw" "Square" "Sine"] (-> params (nth 3) :attrs :labels)))
    (is (true? (-> params second :attrs :value)))
    (is (= ##-Inf (-> gate :content second :attrs :value)) "`-inf` decodes")
    (is (= ##Inf (-> gate :content (nth 2) :attrs :value)) "`inf` decodes")
    (is (str/includes? (dx/emit-str p) "value=\"-inf\"") "infinity encodes back to `-inf`")
    (is (= [:notes :audio] (-> p :content (nth 2) :content first :attrs :contentType)))))

(deftest schemas-for-individual-elements
  (is (ds/validate (ds/schema :dawproject/Note)
                   {:tag :Note :attrs {:time 0.0 :duration 1.0 :channel 0 :key 60}}))
  (is (ds/validate (ds/schema :dawproject.type/unit) :decibel))
  (is (not (ds/validate (ds/schema :dawproject.type/unit) :volts))))

(deftest dawproject-zip
  (let [{:keys [project metadata entries]} (dx/read-dawproject (resource "reference/test.dawproject"))]
    (is (ds/validate ds/StrictProject project))
    (is (ds/validate ds/MetaData metadata))
    (is (= #{"project.xml" "metadata.xml"} (set entries))))
  (let [f (java.io.File/createTempFile "kitchen" ".dawproject")
        p (kitchen-sink)
        m (dx/parse-metadata (resource "metadata.xml"))]
    (try
      (dx/write-dawproject f p m {"audio/drums.wav" (.getBytes "RIFF")})
      (let [r (dx/read-dawproject f)]
        (is (= p (:project r)))
        (is (= m (:metadata r)))
        (is (= #{"project.xml" "metadata.xml" "audio/drums.wav"} (set (:entries r)))))
      (finally (.delete f)))))

;; ---------------------------------------------------------------------------
;; Negative cases
;; ---------------------------------------------------------------------------

(def ^:private minimal
  {:tag :Project
   :attrs {:version "1.0"}
   :content [{:tag :Application :attrs {:name "x" :version "1"}}]})

(defn- with-structure [& items]
  (update minimal :content conj {:tag :Structure :content (vec items)}))

(defn- invalid? [doc] (not (ds/validate doc)))

(deftest minimal-project
  (is (ds/validate minimal))
  (is (invalid? (dissoc minimal :content)) "Application is required")
  (is (invalid? (assoc-in minimal [:attrs] {})) "version is required"))

(deftest element-order-and-cardinality
  (testing "Channel children must follow XSD sequence order"
    (is (ds/validate (with-structure
                       {:tag :Channel :content [{:tag :Pan :attrs {:unit :normalized}}
                                                {:tag :Volume :attrs {:unit :linear}}]})))
    (is (invalid? (with-structure
                    {:tag :Channel :content [{:tag :Volume :attrs {:unit :linear}}
                                             {:tag :Pan :attrs {:unit :normalized}}]}))))
  (testing "Transport before Application is invalid"
    (is (invalid? (assoc minimal :content [{:tag :Transport}
                                           {:tag :Application :attrs {:name "x" :version "1"}}]))))
  (testing "markers needs at least one Marker"
    (is (not (ds/validate (ds/schema :dawproject/markers) {:tag :markers}))))
  (testing "Warps needs a content timeline and at least one Warp"
    (let [audio {:tag :Audio :attrs {:channels 2 :sampleRate 48000 :duration 1.0}
                 :content [{:tag :File :attrs {:path "a.wav"}}]}
          warps (ds/schema :dawproject/Warps)]
      (is (ds/validate warps {:tag :Warps :attrs {:contentTimeUnit :seconds}
                              :content [audio {:tag :Warp :attrs {:time 0.0 :contentTime 0.0}}]}))
      (is (not (ds/validate warps {:tag :Warps :attrs {:contentTimeUnit :seconds}
                                   :content [audio]})))
      (is (not (ds/validate warps {:tag :Warps :attrs {:contentTimeUnit :seconds}
                                   :content [{:tag :Warp :attrs {:time 0.0 :contentTime 0.0}}]})))))
  (testing "Scene needs exactly one timeline"
    (is (not (ds/validate (ds/schema :dawproject/Scene) {:tag :Scene})))))

(deftest attributes
  (testing "unknown attributes are rejected (Clip is nameable, not referenceable)"
    (is (not (ds/validate (ds/schema :dawproject/Clip) {:tag :Clip :attrs {:time 0.0 :id "x"}}))))
  (testing "missing required attribute"
    (is (not (ds/validate (ds/schema :dawproject/RealParameter) {:tag :RealParameter :attrs {:value 1.0}}))))
  (testing "bad enum value"
    (is (invalid? (with-structure {:tag :Channel :attrs {:role :main}}))))
  (testing "wrong value type"
    (is (not (ds/validate (ds/schema :dawproject/Note)
                          {:tag :Note :attrs {:time 0.0 :duration 1.0 :channel 0 :key "C4"}})))))

(deftest choices
  (testing "only device elements are allowed in Devices"
    (is (invalid? (with-structure
                    {:tag :Channel :content [{:tag :Devices :content [{:tag :Track}]}]}))))
  (testing "abstract elements (Timeline, Point, parameter) are not instantiable"
    (is (not (ds/validate (ds/schema :dawproject/Lanes) {:tag :Lanes :content [{:tag :Timeline}]})))))

(deftest references
  (testing "dangling IDREF"
    (let [p (with-structure {:tag :Channel :attrs {:id "a" :destination "nope"}})]
      (is (ds/validate p))
      (is (not (ds/validate ds/StrictProject p)))
      (is (= [{:error :unresolved-idref :tag :Channel :attr :destination :value "nope"}]
             (ds/reference-errors p)))))
  (testing "duplicate ids"
    (let [p (with-structure {:tag :Channel :attrs {:id "a"}} {:tag :Channel :attrs {:id "a"}})]
      (is (= [{:error :duplicate-id :id "a" :count 2}] (ds/reference-errors p))))))

(deftest raw-xml-is-decoded-before-validation
  (let [xml "<Project version=\"1.0\"><Application name=\"a\" version=\"1\"/>
             <Structure><Channel role=\"master\" audioChannels=\"two\"/></Structure></Project>"]
    (is (= {:content [nil {:content [{:attrs {:audioChannels ["should be an integer"]}}]}]}
           (ds/humanize (dx/parse-project xml))))))

(deftest error-messages
  (testing "misplaced element"
    (is (= {:content [nil {:content [{:content [nil ["unexpected element order or count"]]}]}]}
           (ds/humanize (with-structure
                          {:tag :Channel :content [{:tag :Volume :attrs {:unit :linear}}
                                                   {:tag :Pan :attrs {:unit :normalized}}]})))))
  (testing "element not allowed here"
    (is (= {:content [nil {:content [{:tag ["unexpected element :Note, expected one of: Track, Channel"
                                            "should be either :Track or :Channel"]
                                      :malli/error ["unexpected element order or count"]}]}]}
           (ds/humanize (with-structure {:tag :Note})))))
  (testing "deeply nested attribute error (Channel/Sends/Send@type)"
    (is (= {:content [nil {:content [{:content [nil nil {:content [{:attrs {:type ["should be either :pre or :post"]}}]}]}]}]}
           (ds/humanize (with-structure
                          {:tag :Channel
                           :content [{:tag :Mute :attrs {:value false}}
                                     {:tag :Pan :attrs {:unit :normalized}}
                                     {:tag :Sends :content [{:tag :Send :attrs {:type :sideways}
                                                             :content [{:tag :Volume :attrs {:unit :linear}}]}]}]}))))))
