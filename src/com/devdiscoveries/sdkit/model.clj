(ns com.devdiscoveries.sdkit.model
  (:require [clojure.spec.alpha :as spec]
            [clojure.walk :as w]
            [clojure.string :as s]))


(spec/def ::initial-time number?)
(spec/def ::timestep number?)
(spec/def ::final-time number?)
(spec/def ::name string?)
(spec/def ::metadata (spec/keys :req-un [::initial-time
                                      ::final-time
                                      ::timestep
                                      ::name]))

(spec/def ::stocks vector?)
(spec/def ::flows vector?)
(spec/def ::converters vector?)
(spec/def ::constants vector?)
(spec/def ::model (spec/keys :req-un [::metadata
                                         ::stocks
                                         ::flows
                                         ::converters
                                         ::constants]))

(defprotocol ModelEntity
  "Protocol defining the methods that Model Entities must implement. For every Model Entity one can talk about it's value and/or the differential on that value, based on some world state.
We have the following entities:
   - Constant: representing a fixed value during the whole lifetime of the simulation.
   - Stock: representing an reservoir of something that has to be tracked.
   - Converter: Defines a boundary condition of the model, or can be calculated from other entities with a formula.
   - Flow: Defines how one Stock flows into another."
  (id [self] "Returns key for use in world-state.")
  (value [self state] "Returns the current value based on a certain world state. `state` is a map.")
  (differential [self state] "Returns the differential to the value based on a certain world state. `state` is a map."))

(defrecord Constant [const-id default-value]
  ModelEntity
  (id [self] const-id)
  (value [self state] (const-id state))
  (differential [self state] 0.0))

(defrecord Converter [conv-id formula args]
  ModelEntity
  (id [self] conv-id)
  (value [self state]
    (let [eval-args (map (fn [e] (e state)) args)]
      (apply formula eval-args)))
  (differential [self state] nil))

(defrecord Stock [stock-id default-value formula args]
  ModelEntity
  (id [self] stock-id)
  (value [self state] (stock-id state))
  (differential [self state]
    (let [eval-args (map (fn [e] (e state)) args)]
      (apply formula eval-args))))

(defrecord Flow [flow-id formula args]
  ModelEntity
  (id [self] flow-id)
  (value [self state]
    (let [eval-args (map (fn [e] (e state)) args)]
      (apply formula eval-args)))
  (differential [self state] nil))


(defrecord Model [metadata stocks flows converters constants])


(defn add-entity
  "Convenience function to add a model entity to a world state."
  [state entity]
  (swap! state (fn [m] (assoc m (id entity) entity))))

(defn print-state!
  "Convenience method to print the current world state for debug and logging purposes."
  [state]
  (if (empty? state)
    (println "State is empty.")
    (doseq [[k v]  state] (println k "=" v))))

(defn add-constants [state constants]
  (if (not (empty? constants))
    (let [c (first constants)]
      (recur (assoc state (id c) (:default-value c)) (rest constants)))
    state))

(defn add-stocks [state last-state stocks integrator-fn]
  (if (not (empty? stocks))
    (let [s (first stocks)
          value (if last-state
                  (integrator-fn last-state s)
                  (:default-value s))]
      (recur (assoc state (id s) value)
             last-state
             (rest stocks)
             integrator-fn))
    state))

(defn add-converters [state converters]
  (if (not (empty? converters))
    (let [c (first converters)
          value (value c state)]
      (recur (assoc state (id c) value)
             (rest converters)))
    state))

(defn add-flows [state flows]
  (if (not (empty? flows))
    (let [f (first flows)
          value (value f state)]
      (recur (assoc state (id f) value)
             (rest flows)))
    state))

(defn euler-integrator [state stock]
  (let [sid (id stock)]
    (+ (sid state) (differential stock state))))

(defn initial-state [model]
  (-> {}
      (add-constants (:constants model))
      (add-stocks nil (:stocks model) nil)
      (add-converters (:converters model))
      (add-flows (:flows model))))


(defn- char-range
  "Helper function that returns a vector of characters, starting with 'a', 'b', etc."
  [chars-needed]
  (into []
        (map (fn [i] (->> i
                          char
                          str
                          symbol))
             (range (int \a) (+ (int \a) chars-needed)))))


(defmacro defconst [const-name default-value]
  "Macro that makes defining a Constant and adding it to the world state easier."
  `(do
     (def ~const-name (->Constant ~(keyword const-name) ~default-value))
     ~const-name))


(defmacro defconv [conv-name formula]
  "Macro that makes defining a Converter and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Converter. "
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~conv-name
         (->Converter ~(keyword conv-name)
                      ~(eval (w/postwalk-replace args-map formula))
                      ~(into [] (map (fn [a] (keyword a)) args))))
       ~conv-name)))


(defmacro defstock [stock-name default-value formula]
    "Macro that makes defining a Stock and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Stock. "
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~stock-name
         (->Stock ~(keyword stock-name)
                  ~default-value
                  ~(eval (w/postwalk-replace args-map formula))
                  ~(into [] (map (fn [a] (keyword a)) args))))
       ~stock-name)))


(defmacro defflow [flow-name formula]
    "Macro that makes defining a Flow and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Flow. "
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~flow-name
         (->Flow ~(keyword flow-name)
                 ~(eval (w/postwalk-replace args-map formula))
                 ~(into [] (map (fn [a] (keyword a)) args))))
       ~flow-name)))


(defn filter-model-entities [macro coll]
    (into [] (filter (fn [e] (s/includes? (first e) (str macro))) coll)))

(defmacro defmodel [model-name model-metadata & body]
  (let [stocks (filter-model-entities 'defstock body)
        flows (filter-model-entities 'defflow body)
        converters (filter-model-entities 'defconv body)
        consts (filter-model-entities 'defconst body)]
    `(do
       (def ~model-name (->Model ~model-metadata ~stocks ~flows ~converters ~consts))
       ~model-name)))
