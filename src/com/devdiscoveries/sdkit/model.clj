(ns com.devdiscoveries.sdkit.model
  (:require [clojure.spec.alpha :as spec]))


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
  "Protocol defining the methods that Model Entities must implement.
For every Model Entity one can talk about it's value and/or the
differential on that value, based on some world state.
We have the following entities:
 - Constant: representing a fixed value during the whole lifetime of the simulation.
 - Stock: representing an reservoir of something that has to be tracked.
 - Converter: Defines a boundary condition of the model, or can be calculated from other entities with a formula.
 - Flow: Defines how one Stock flows into another."
  (id [self] "Returns key for use in world-state.")
  (value [self state] "Returns the current value based on a certain world state. `state` is a map."))

(defn- apply-formula
  "Applies a formula for a `ModelEntity` to the value based on a certain world state. Such a `ModelEntity` must have `:args` and `:formula` properties. `state` is a map."
  [entity state]
  (let [eval-args (map (fn [e] (e state)) (:args entity))]
    (apply (:formula entity) eval-args)))


(defrecord Constant [const-id default-value]
  ModelEntity
  (id [self] const-id)
  (value [self state] (const-id state)))

(defrecord Converter [conv-id formula args]
  ModelEntity
  (id [self] conv-id)
  (value [self state]
    (let [eval-args (map (fn [e] (e state)) args)]
      (apply formula eval-args))))

(defrecord Stock [stock-id default-value formula args]
  ModelEntity
  (id [self] stock-id)
  (value [self state] (stock-id state)))

(defn differential
  "Returns the differential for a `Stock` to the value based on a certain world state. `state` is a map."
  [stock state] (apply-formula stock state))

(defrecord Flow [flow-id formula args]
  ModelEntity
  (id [self] flow-id)
  (value [self state]
    (let [eval-args (map (fn [e] (e state)) args)]
      (apply formula eval-args))))


(defrecord Model [metadata stocks flows converters constants])
