(ns dawproject.schema
  "Malli schemas for the DAWproject 1.0 format (Project.xsd + MetaData.xsd).

  Data model
  ----------
  Every XML element is a map shaped like a `clojure.data.xml` element:

      {:tag     :Track
       :attrs   {:id \"id8\" :name \"Track 1\" :contentType [:notes] :loaded true}
       :content [{:tag :Channel ...}]}

  * `:tag`     – the XML element name, as a keyword (exactly as in the XSD).
  * `:attrs`   – XML attributes, keyed by keyword. Optional when the element has
                 no required attributes.
  * `:content` – child elements, in document order. Sequence/choice rules of the
                 XSD are expressed with Malli regex schemas, so element order and
                 cardinality are validated. Optional when it may be empty.

  Attribute values are *typed*: ints, doubles (incl. ##Inf / ##-Inf), booleans,
  enum values as keywords and `xs:list` attributes as vectors. Use
  `xml-transformer` (or `dawproject.xml`) to convert between raw XML strings and
  this typed representation.

  Schema names
  ------------
  Every global element in the XSD is registered as `:dawproject/<ElementName>`,
  e.g. `:dawproject/Project`, `:dawproject/MetaData`, `:dawproject/Track`.
  Simple types are registered as `:dawproject.type/<typeName>`, e.g.
  `:dawproject.type/unit`."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]))

;; ---------------------------------------------------------------------------
;; Simple types
;; ---------------------------------------------------------------------------

(defn- parse-double*
  "Parses a DAWproject double. Mirrors com.bitwig.dawproject.DoubleAdapter,
  which writes infinities as `inf` / `-inf`."
  [x]
  (if (string? x)
    (let [s (str/trim x)]
      (case s
        ("inf" "+inf") ##Inf
        "-inf" ##-Inf
        (or (parse-double s) x)))
    x))

(defn- format-double*
  "Formats a double the way the reference implementation does (`%.6f`, `inf`)."
  [x]
  (if (double? x)
    (cond
      (= x ##Inf) "inf"
      (= x ##-Inf) "-inf"
      :else (String/format java.util.Locale/US "%.6f" (object-array [x])))
    x))

(defn- split-list [x]
  (if (string? x)
    (let [s (str/trim x)] (if (= "" s) [] (str/split s #"\s+")))
    x))

(defn- join-list [x]
  (if (sequential? x) (str/join " " (map #(if (keyword? %) (name %) (str %)) x)) x))

(defn- enum [& values]
  (into [:enum] (map keyword) values))

(def ^:private simple-types
  {;; XSD built-ins
   :dawproject.type/string  :string
   :dawproject.type/int     [:int {:min Integer/MIN_VALUE :max Integer/MAX_VALUE}]
   :dawproject.type/boolean :boolean
   ;; DAWproject doubles are written as xs:string or xs:double in the XSD but are
   ;; always Java Doubles serialized via DoubleAdapter, which allows `inf`.
   :dawproject.type/double  [:double {:decode/xml parse-double*
                                      :encode/xml format-double*}]
   ;; xs:ID / xs:IDREF (NCName)
   :dawproject.type/ID      [:re {:error/message "should be an XML NCName (xs:ID)"}
                             #"^[\p{L}_][\p{L}\p{N}_.\-·]*$"]
   :dawproject.type/IDREF   [:re {:error/message "should be an XML NCName (xs:IDREF)"}
                             #"^[\p{L}_][\p{L}\p{N}_.\-·]*$"]

   ;; Enumerations from Project.xsd
   :dawproject.type/unit
   (enum "linear" "normalized" "percent" "decibel" "hertz" "semitones" "seconds" "beats" "bpm")
   :dawproject.type/timeUnit       (enum "beats" "seconds")
   :dawproject.type/deviceRole     (enum "instrument" "noteFX" "audioFX" "analyzer")
   :dawproject.type/eqBandType
   (enum "highPass" "lowPass" "bandPass" "highShelf" "lowShelf" "bell" "notch")
   :dawproject.type/mixerRole      (enum "regular" "master" "effect" "submix" "vca")
   :dawproject.type/sendType       (enum "pre" "post")
   :dawproject.type/contentType
   (enum "audio" "automation" "notes" "video" "markers" "tracks")
   :dawproject.type/interpolation  (enum "hold" "linear")
   :dawproject.type/expressionType
   (enum "gain" "pan" "transpose" "timbre" "formant" "pressure" "channelController"
         "channelPressure" "polyPressure" "pitchBend" "programChange")

   ;; xs:list attributes
   :dawproject.type/contentTypeList
   [:vector {:decode/xml split-list :encode/xml join-list} :dawproject.type/contentType]
   :dawproject.type/stringList
   [:vector {:decode/xml split-list :encode/xml join-list} :string]})

;; ---------------------------------------------------------------------------
;; Complex types
;;
;; A complex type is plain data: {:attrs [map-entries...] :content [regex parts...]}.
;; `xs:extension` appends attributes and appends the content sequence, exactly
;; like XSD does. Elements are then built from a tag + a complex type.
;; ---------------------------------------------------------------------------

(defn- opt [k t & [props]] [k (merge {:optional true} props) t])
(defn- req [k t & [props]] (if props [k props t] [k t]))

(defn- ext
  "XSD `xs:extension`: base type + extra attributes + appended content."
  [base & {:keys [attrs content]}]
  {:attrs (into (vec (:attrs base)) attrs)
   :content (into (vec (:content base)) content)})

(defn- el-ref
  "Reference to a global element. Wrapped in :schema so Malli treats it as a
  single item inside regex (sequence) schemas, which allows recursion."
  [tag]
  [:schema [:ref (keyword "dawproject" (name tag))]])

(defn- choice
  "An `xs:choice` of global elements (used as a single content item)."
  [& tags]
  (into [:multi {:dispatch :tag}] (map (fn [t] [t (el-ref t)])) tags))

(defn- regex-op? [x] (and (vector? x) (contains? #{:? :* :+} (first x))))

(defn- leaf-tags
  "Element names a content leaf (element schema, el-ref or choice) can match."
  [leaf]
  (case (first leaf)
    :map    [(get-in leaf [2 1 1])]               ; [:map props [:tag [:= T]] ...]
    :schema [(keyword (name (get-in leaf [1 1])))] ; [:schema [:ref :dawproject/T]]
    :multi  (mapv first (drop 2 leaf))))

(defn- content-schema
  "Builds the `:content` schema from a sequence of content parts, each being
  a leaf or `[:? leaf]`, `[:* leaf]`, `[:+ leaf]`.

  Validation happens in two layers, which keeps error messages precise:
  1. every child is validated against its own element schema (dispatch on :tag),
  2. order and cardinality are validated with a regex over the tags only."
  [parts]
  (if (empty? parts)
    [:cat]
    (let [tag-matcher (fn [leaf]
                        (let [ts (leaf-tags leaf)]
                          [:map [:tag (if (= 1 (count ts)) [:= (first ts)] (into [:enum] ts))]]))
          order (into [:cat {:error/message "unexpected element order or count"}]
                      (map (fn [p] (if (regex-op? p)
                                     [(first p) (tag-matcher (second p))]
                                     (tag-matcher p))))
                      parts)
          leaves (map #(if (regex-op? %) (second %) %) parts)
          by-tag (reduce (fn [acc leaf]
                           (reduce (fn [acc t]
                                     (assoc acc t (if (= :multi (first leaf)) (el-ref t) leaf)))
                                   acc (leaf-tags leaf)))
                         (array-map) leaves)
          allowed (str/join ", " (map name (keys by-tag)))
          items (into [:multi {:dispatch :tag
                               :error/fn (fn [{:keys [value]} _]
                                           (str "unexpected element "
                                                (pr-str (if (map? value) (:tag value) value))
                                                ", expected one of: " allowed))}]
                      by-tag)]
      [:and [:sequential items] order])))

(defn- element
  "Builds the schema for an element named `tag` of complex type `type`."
  ([tag type] (element tag type nil))
  ([tag {:keys [attrs content]} props]
   (let [attrs-schema (into [:map {:closed true}] attrs)
         attrs-required? (some (fn [[_ p]] (not (and (map? p) (:optional p)))) attrs)
         ;; content may be omitted iff every part is optional (:? or :*)
         content-required? (some #(not (#{:? :*} (when (vector? %) (first %)))) content)]
     [:map (merge {:closed true} props)
      [:tag [:= tag]]
      (if attrs-required? [:attrs attrs-schema] [:attrs {:optional true} attrs-schema])
      (if content-required?
        [:content (content-schema content)]
        [:content {:optional true} (content-schema content)])])))

;; -- abstract bases ---------------------------------------------------------

(def ^:private nameable
  {:attrs [(opt :name :string)
           (opt :color :string)
           (opt :comment :string)]
   :content []})

(def ^:private referenceable
  (ext nameable :attrs [(opt :id :dawproject.type/ID)]))

(def ^:private parameter
  (ext referenceable :attrs [(opt :parameterID :dawproject.type/int)]))

(def ^:private lane referenceable)

(def ^:private timeline
  (ext referenceable :attrs [(opt :timeUnit :dawproject.type/timeUnit)
                             (opt :track :dawproject.type/IDREF)]))

(def ^:private point
  {:attrs [(req :time :dawproject.type/double)] :content []})

;; -- parameters --------------------------------------------------------------

(def ^:private real-parameter
  (ext parameter :attrs [(opt :max :dawproject.type/double)
                         (opt :min :dawproject.type/double)
                         (req :unit :dawproject.type/unit)
                         (opt :value :dawproject.type/double)]))

(def ^:private bool-parameter
  (ext parameter :attrs [(opt :value :dawproject.type/boolean)]))

(def ^:private integer-parameter
  (ext parameter :attrs [(opt :max :dawproject.type/int)
                         (opt :min :dawproject.type/int)
                         (opt :value :dawproject.type/int)]))

(def ^:private enum-parameter
  (ext parameter :attrs [(req :count :dawproject.type/int)
                         (opt :labels :dawproject.type/stringList)
                         (opt :value :dawproject.type/int)]))

(def ^:private time-signature-parameter
  (ext parameter :attrs [(req :denominator :dawproject.type/int)
                         (req :numerator :dawproject.type/int)]))

;; -- misc --------------------------------------------------------------------

(def ^:private application
  {:attrs [(req :name :string) (req :version :string)] :content []})

(def ^:private file-reference
  {:attrs [(req :path :string) (opt :external :dawproject.type/boolean)] :content []})

(def ^:private transport
  {:attrs []
   :content [[:? (element :Tempo real-parameter)]
             [:? (element :TimeSignature time-signature-parameter)]]})

(defn- wrapper
  "Anonymous complex type with no attributes that only holds a list of children."
  [tag item]
  (element tag {:attrs [] :content [[:* item]]}))

;; -- the timeline content choice, reused in many places ------------------------
;; (The XSD also lists the abstract `Timeline` element here; it can never be
;; instantiated without xsi:type, so it is omitted.)

(def ^:private timeline-choice
  (choice :Lanes :Notes :Clips :ClipSlot :markers :Warps :Audio :Video :Points))

;; -- structure ---------------------------------------------------------------

(def ^:private send-type
  (ext referenceable
       :attrs [(opt :destination :dawproject.type/IDREF)
               (opt :type :dawproject.type/sendType)]
       :content [[:? (element :Enable bool-parameter)]
                 [:? (element :Pan real-parameter)]
                 (element :Volume real-parameter)]))

(def ^:private device-choice
  (choice :Device :Vst2Plugin :Vst3Plugin :ClapPlugin :BuiltinDevice
          :Equalizer :Compressor :NoiseGate :Limiter :AuPlugin))

(def ^:private channel
  (ext lane
       :attrs [(opt :audioChannels :dawproject.type/int)
               (opt :destination :dawproject.type/IDREF)
               (opt :role :dawproject.type/mixerRole)
               (opt :solo :dawproject.type/boolean)]
       :content [[:? (wrapper :Devices device-choice)]
                 [:? (element :Mute bool-parameter)]
                 [:? (element :Pan real-parameter)]
                 [:? (wrapper :Sends (element :Send send-type))]
                 [:? (element :Volume real-parameter)]]))

(def ^:private track
  (ext lane
       :attrs [(opt :contentType :dawproject.type/contentTypeList)
               (opt :loaded :dawproject.type/boolean)]
       :content [[:? (el-ref :Channel)]
                 [:* (el-ref :Track)]]))

;; -- devices -----------------------------------------------------------------
;; (The abstract `parameter` element is omitted from the choice, see above.)

(def ^:private parameter-choice
  (choice :RealParameter :BoolParameter :IntegerParameter :EnumParameter
          :TimeSignatureParameter))

(def ^:private device
  (ext referenceable
       :attrs [(opt :deviceID :string)
               (req :deviceName :string)
               (req :deviceRole :dawproject.type/deviceRole)
               (opt :deviceVendor :string)
               (opt :loaded :dawproject.type/boolean)]
       :content [[:? (wrapper :Parameters parameter-choice)]
                 [:? (element :Enabled bool-parameter)]
                 [:? (element :State file-reference)]]))

(def ^:private plugin
  (ext device :attrs [(opt :pluginVersion :string)]))

(def ^:private builtin-device device)

(def ^:private eq-band
  {:attrs [(req :type :dawproject.type/eqBandType)
           (opt :order :dawproject.type/int)]
   :content [(element :Freq real-parameter)
             [:? (element :Gain real-parameter)]
             [:? (element :Q real-parameter)]
             [:? (element :Enabled bool-parameter)]]})

(def ^:private equalizer
  (ext builtin-device
       :content [[:* (element :Band eq-band)]
                 [:? (element :InputGain real-parameter)]
                 [:? (element :OutputGain real-parameter)]]))

(def ^:private compressor
  (ext builtin-device
       :content [[:? (element :Attack real-parameter)]
                 [:? (element :AutoMakeup bool-parameter)]
                 [:? (element :InputGain real-parameter)]
                 [:? (element :OutputGain real-parameter)]
                 [:? (element :Ratio real-parameter)]
                 [:? (element :Release real-parameter)]
                 [:? (element :Threshold real-parameter)]]))

(def ^:private noise-gate
  (ext builtin-device
       :content [[:? (element :Attack real-parameter)]
                 [:? (element :Range real-parameter)]
                 [:? (element :Ratio real-parameter)]
                 [:? (element :Release real-parameter)]
                 [:? (element :Threshold real-parameter)]]))

(def ^:private limiter
  (ext builtin-device
       :content [[:? (element :Attack real-parameter)]
                 [:? (element :InputGain real-parameter)]
                 [:? (element :OutputGain real-parameter)]
                 [:? (element :Release real-parameter)]
                 [:? (element :Threshold real-parameter)]]))

;; -- timelines ---------------------------------------------------------------

(def ^:private lanes
  (ext timeline :content [[:* timeline-choice]]))

(def ^:private note
  {:attrs [(req :time :dawproject.type/double)
           (req :duration :dawproject.type/double)
           (req :channel :dawproject.type/int)
           (req :key :dawproject.type/int)
           (opt :vel :dawproject.type/double)
           (opt :rel :dawproject.type/double)]
   :content [[:? timeline-choice]]})

(def ^:private notes
  (ext timeline :content [[:* (el-ref :Note)]]))

(def ^:private clip
  (ext nameable
       :attrs [(req :time :dawproject.type/double)
               (opt :duration :dawproject.type/double)
               (opt :contentTimeUnit :dawproject.type/timeUnit)
               (opt :playStart :dawproject.type/double)
               (opt :playStop :dawproject.type/double)
               (opt :loopStart :dawproject.type/double)
               (opt :loopEnd :dawproject.type/double)
               (opt :fadeTimeUnit :dawproject.type/timeUnit)
               (opt :fadeInTime :dawproject.type/double)
               (opt :fadeOutTime :dawproject.type/double)
               (opt :enable :dawproject.type/boolean)
               (opt :reference :dawproject.type/IDREF)]
       :content [[:? timeline-choice]]))

(def ^:private clips
  (ext timeline :content [[:* (el-ref :Clip)]]))

(def ^:private clip-slot
  (ext timeline
       :attrs [(opt :hasStop :dawproject.type/boolean)]
       :content [[:? (el-ref :Clip)]]))

(def ^:private marker
  (ext nameable :attrs [(req :time :dawproject.type/double)]))

(def ^:private markers
  (ext timeline :content [[:+ (el-ref :Marker)]]))

(def ^:private warp
  {:attrs [(req :time :dawproject.type/double)
           (req :contentTime :dawproject.type/double)]
   :content []})

(def ^:private warps
  (ext timeline
       :attrs [(req :contentTimeUnit :dawproject.type/timeUnit)]
       :content [timeline-choice
                 [:+ (el-ref :Warp)]]))

(def ^:private media-file
  (ext timeline
       :attrs [(req :duration :dawproject.type/double)]
       :content [(element :File file-reference)]))

(def ^:private audio
  (ext media-file :attrs [(opt :algorithm :string)
                          (req :channels :dawproject.type/int)
                          (req :sampleRate :dawproject.type/int)]))

(def ^:private video
  (ext media-file :attrs [(opt :algorithm :string)
                          (req :channels :dawproject.type/int)
                          (req :sampleRate :dawproject.type/int)]))

(def ^:private real-point
  (ext point :attrs [(req :value :dawproject.type/double)
                     (opt :interpolation :dawproject.type/interpolation)]))

(def ^:private enum-point
  (ext point :attrs [(req :value :dawproject.type/int)]))

(def ^:private bool-point
  (ext point :attrs [(req :value :dawproject.type/boolean)]))

(def ^:private integer-point
  (ext point :attrs [(req :value :dawproject.type/int)]))

(def ^:private time-signature-point
  (ext point :attrs [(req :numerator :dawproject.type/int)
                     (req :denominator :dawproject.type/int)]))

(def ^:private automation-target
  {:attrs [(opt :parameter :dawproject.type/IDREF)
           (opt :expression :dawproject.type/expressionType)
           (opt :channel :dawproject.type/int)
           (opt :key :dawproject.type/int)
           (opt :controller :dawproject.type/int)]
   :content []})

;; (The abstract `Point` element is omitted from the choice, see above.)
(def ^:private points
  (ext timeline
       :attrs [(opt :unit :dawproject.type/unit)]
       :content [(element :Target automation-target)
                 [:* (choice :RealPoint :EnumPoint :BoolPoint :IntegerPoint
                             :TimeSignaturePoint)]]))

;; -- arrangement / scenes / project -------------------------------------------

(def ^:private arrangement
  (ext referenceable
       :content [[:? (el-ref :Lanes)]
                 [:? (element :Markers markers)]
                 [:? (element :TempoAutomation points)]
                 [:? (element :TimeSignatureAutomation points)]]))

(def ^:private scene
  (ext referenceable :content [timeline-choice]))

(def ^:private project
  {:attrs [(req :version :string)]
   :content [(element :Application application)
             [:? (element :Transport transport)]
             [:? (wrapper :Structure (choice :Track :Channel))]
             [:? (el-ref :Arrangement)]
             [:? (wrapper :Scenes (el-ref :Scene))]]})

;; -- MetaData.xsd --------------------------------------------------------------

(defn- text-element [tag]
  [:map {:closed true}
   [:tag [:= tag]]
   [:attrs {:optional true} [:map {:closed true}]]
   [:content {:optional true} [:* :string]]])

(def ^:private metadata-fields
  [:Title :Artist :Album :OriginalArtist :Composer :Songwriter :Producer :Arranger
   :Year :Genre :Copyright :Website :Comment])

(def ^:private metadata
  {:attrs []
   :content (mapv (fn [t] [:? (text-element t)]) metadata-fields)})

;; ---------------------------------------------------------------------------
;; Registry
;; ---------------------------------------------------------------------------

(def ^:private global-elements
  "All global (top-level, non-abstract) elements of Project.xsd and MetaData.xsd."
  {:Project                project
   :MetaData               metadata
   :Arrangement            arrangement
   :Scene                  scene
   :Track                  track
   :Channel                channel
   ;; devices
   :Device                 device
   :BuiltinDevice          builtin-device
   :Vst2Plugin             plugin
   :Vst3Plugin             plugin
   :ClapPlugin             plugin
   :AuPlugin               plugin
   :Equalizer              equalizer
   :Compressor             compressor
   :NoiseGate              noise-gate
   :Limiter                limiter
   ;; parameters
   :RealParameter          real-parameter
   :BoolParameter          bool-parameter
   :IntegerParameter       integer-parameter
   :EnumParameter          enum-parameter
   :TimeSignatureParameter time-signature-parameter
   ;; timelines
   :Lanes                  lanes
   :Notes                  notes
   :Note                   note
   :Clips                  clips
   :Clip                   clip
   :ClipSlot               clip-slot
   :markers                markers
   :Marker                 marker
   :Warps                  warps
   :Warp                   warp
   :Audio                  audio
   :Video                  video
   :Points                 points
   ;; automation points
   :RealPoint              real-point
   :EnumPoint              enum-point
   :BoolPoint              bool-point
   :IntegerPoint           integer-point
   :TimeSignaturePoint     time-signature-point})

(def element-tags
  "Names of all global (instantiable) elements, e.g. :Project, :Track."
  (set (keys global-elements)))

(def registry
  "Malli registry containing the default schemas plus all DAWproject schemas."
  (merge (m/default-schemas)
         simple-types
         (into {}
               (map (fn [[tag type]]
                      [(keyword "dawproject" (name tag)) (element tag type)]))
               global-elements)))

(def options {:registry registry})

(defn schema
  "Returns the compiled schema for `k`, e.g. `:dawproject/Project`."
  [k]
  (m/schema k options))

(def Project  (schema :dawproject/Project))
(def MetaData (schema :dawproject/MetaData))

;; ---------------------------------------------------------------------------
;; Transformer
;; ---------------------------------------------------------------------------

(def xml-transformer
  "Converts between raw XML attribute strings (as produced by clojure.data.xml)
  and the typed representation. Use with `m/decode` / `m/encode`."
  (mt/transformer {:name :xml} (mt/string-transformer)))

;; ---------------------------------------------------------------------------
;; Referential integrity (xs:ID / xs:IDREF)
;; ---------------------------------------------------------------------------

(def ^:private idref-attrs
  "IDREF attributes per element tag."
  {:track #{:Lanes :Notes :Clips :ClipSlot :markers :Markers :Warps :Audio :Video
            :Points :TempoAutomation :TimeSignatureAutomation}
   :destination #{:Channel :Send}
   :reference #{:Clip}
   :parameter #{:Target}})

(defn- elements [node]
  (tree-seq #(seq (:content %)) #(filter map? (:content %)) node))

(defn reference-errors
  "Checks xs:ID uniqueness and that every xs:IDREF points to an existing id.
  Returns a (possibly empty) vector of error maps."
  [doc]
  (let [els (elements doc)
        ids (keep #(get-in % [:attrs :id]) els)
        id-set (set ids)
        dupes (for [[id n] (frequencies ids) :when (> n 1)]
                {:error :duplicate-id :id id :count n})
        dangling (for [el els
                       [attr tags] idref-attrs
                       :when (contains? tags (:tag el))
                       :let [v (get-in el [:attrs attr])]
                       :when (and v (not (contains? id-set v)))]
                   {:error :unresolved-idref :tag (:tag el) :attr attr :value v})]
    (into (vec dupes) dangling)))

(def StrictProject
  "Like `Project`, but additionally checks ID uniqueness and IDREF resolution."
  (m/schema [:and :dawproject/Project
             [:fn {:error/fn (fn [{:keys [value]} _]
                               (str "invalid references: "
                                    (pr-str (reference-errors value))))}
              (fn [doc] (empty? (reference-errors doc)))]]
            options))

;; ---------------------------------------------------------------------------
;; Convenience
;; ---------------------------------------------------------------------------

(defn validate
  ([value] (validate Project value))
  ([schema value] (m/validate schema value options)))

(defn explain
  ([value] (explain Project value))
  ([schema value] (m/explain schema value options)))

(defn humanize
  ([value] (humanize Project value))
  ([schema value] (some-> (explain schema value) me/humanize)))
