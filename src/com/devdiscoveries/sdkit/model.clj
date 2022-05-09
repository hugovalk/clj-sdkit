(ns com.devdiscoveries.sdkit.model
  (:require [clojure.spec.alpha :as spec]
            [clojure.walk :as w]))


(spec/def ::initial-time number?)
(spec/def ::timestep number?)
(spec/def ::final-time number?)
(spec/def ::name string?)
(spec/def ::model (spec/keys :req [::initial-time
                                   ::final-time
                                   ::timestep
                                   ::name]))

(defprotocol ModelEntity
  "Protocol defining the methods that Model Entities must implement. For every Model Entity one can talk about it's value and/or the differential on that value, based on some world state.
We have the following entities:
   - Constant: representing a fixed value during the whole lifetime of the simulation.
   - Stock: representing an reservoir of something that has to be tracked.
   - Converter: Defines a boundary condition of the model, or can be calculated from other entities with a formula.
   - Flow: Defines how one Stock flows into another."
  (id [self] "Returns key for use in world-state.")
  (value [self state] "Returns the current value.")
  (differential [self state] "Returns the differential to the value."))

(defrecord Constant [const-id internal-value]
  ModelEntity
  (id [self] const-id)
  (value [self state] @internal-value)
  (differential [self state] 0.0))

(defrecord Converter [conv-id formula args]
  ModelEntity
  (id [self] conv-id)
  (value [self state]
    (let [eval-args (map (fn [e] (value (e @state) state)) args)]
      (apply formula eval-args)))
  (differential [self state] nil))

(defrecord Stock [stock-id current-value formula args]
  ModelEntity
  (id [self] stock-id)
  (value [self state] @current-value)
  (differential [self state]
    (let [eval-args (map (fn [e] (value (e @state) state)) args)]
      (apply formula eval-args))))

(defrecord Flow [flow-id formula args]
  ModelEntity
  (id [self] flow-id)
  (value [self state]
    (let [eval-args (map (fn [e] (value (e @state) state)) args)]
      (apply formula eval-args)))
  (differential [self state] nil))


(defn add-entity [state entity]
  "Convenience function to add a model entity to a world state."
  (swap! state (fn [m] (assoc m (id entity) entity))))

(defn print-state! [state]
  "Convenience method to print the current world state for debug and logging purposes."
  (doseq [[k v] @state] (println k "=" (value v state))))

(defn- char-range [chars-needed]
  "Helper function that returns a vector of characters, starting with 'a', 'b', etc."
  (into []
        (map (fn [i] (->> i
                          char
                          str
                          symbol))
             (range (int \a) (+ (int \a) chars-needed)))))


(defmacro defconst [const-name state value]
  "Macro that makes defining a Constant and adding it to the world state easier."
  `(do
     (def ~const-name (->Constant ~(keyword const-name) (atom ~value)))
     (add-entity ~state ~const-name)
     ~const-name))

(defmacro defconv [conv-name state formula]
  "Macro that makes defining a Converter and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Converter. "
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~conv-name
         (->Converter ~(keyword conv-name)
                      ~(eval (w/postwalk-replace args-map formula))
                      ~(into [] (map (fn [a] (keyword a)) args))))
       (add-entity ~state ~conv-name)
       ~conv-name)))


(defmacro defstock [stock-name state initial-value formula]
    "Macro that makes defining a Stock and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Stock. "
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~stock-name
         (->Stock ~(keyword stock-name)
                  (atom ~initial-value)
                  ~(eval (w/postwalk-replace args-map formula))
                  ~(into [] (map (fn [a] (keyword a)) args))))
       (add-entity ~state ~stock-name)
       ~stock-name)))


(defmacro defflow [flow-name state formula]
    "Macro that makes defining a Flow and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Flow. "
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~flow-name
         (->Flow ~(keyword flow-name)
                 ~(eval (w/postwalk-replace args-map formula))
                 ~(into [] (map (fn [a] (keyword a)) args))))
       (add-entity ~state ~flow-name)
       ~flow-name)))
