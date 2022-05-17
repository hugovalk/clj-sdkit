(ns com.devdiscoveries.sdkit.simulation
  (:require [com.devdiscoveries.sdkit.model :as mod]
            [com.devdiscoveries.sdkit.event-handler :as ev]
            [clojure.tools.logging :refer [info]]
            [clojure.spec.alpha :as spec]))

(spec/def ::total-timesteps number?)
(spec/def ::timesteps-needed number?)
(spec/def ::state-metadata (spec/keys :req [::timesteps-needed
                                            ::total-timesteps]))
(spec/def ::world-state (spec/keys :req [::state-metadata]))

(defn- timesteps-needed [model]
  (let [start (:initial-time model)
        end (:final-time model)
        step (:timestep model)]
    (/ (- end start) step)))

(defn get-metadata [state key]
  (get-in state [::state-metadata key]))


(defn print-state!
  "Convenience method to print the current world state for debug and logging purposes."
  [state]
  (if (empty? state)
    (println "State is empty.")
    (doseq [[k v]  state] (println k "=" v))))

(defn- add-constants [state constants]
  (if (not (empty? constants))
    (let [c (first constants)]
      (recur (assoc state (mod/id c) (:default-value c)) (rest constants)))
    state))

(defn- add-stocks [state last-state stocks integrator-fn]
  (if (not (empty? stocks))
    (let [s (first stocks)
          value (if last-state
                  (integrator-fn last-state s)
                  (:default-value s))]
      (recur (assoc state (mod/id s) value)
             last-state
             (rest stocks)
             integrator-fn))
    state))

(defn- add-converters [state converters]
  (if (not (empty? converters))
    (let [c (first converters)
          value (mod/value c state)]
      (recur (assoc state (mod/id c) value)
             (rest converters)))
    state))

(defn- add-flows [state flows]
  (if (not (empty? flows))
    (let [f (first flows)
          value (mod/value f state)]
      (recur (assoc state (mod/id f) value)
             (rest flows)))
    state))

(defn euler-integrator [state stock]
  (let [sid (mod/id stock)]
    (+ (sid state) (mod/differential stock state))))

(defn setup-initial-state [model]
  (-> {}
      (add-constants (:constants model))
      (add-stocks nil (:stocks model) nil)
      (add-converters (:converters model))
      (add-flows (:flows model))))


(defn initialize-simulation-run [model handler]
  "Initializing the simulation run. Returns an atom, containing the initial state of the simulation."
  (let [initial-state {}
        initial-state {::state-metadata {::total-timesteps 0
                                         ::timesteps-needed (timesteps-needed model)}}]
    (ev/simulation-initialized handler initial-state)
    initial-state))

(defn running-simulation-timesteps [model handler current-state]
  "Running the next timestep of the model. Returns the updated state of the simulation."
  (info current-state)
  (if (> (get-metadata current-state ::timesteps-needed)
          (get-metadata current-state ::total-timesteps))
    (let [updated-state (update-in current-state
                                   [::state-metadata ::total-timesteps]
                                   inc)]
      (ev/timestep-calculated handler updated-state)
      (recur model handler updated-state))
    current-state))

(defn finish-simulation-run [model handler current-state]
  "Finishing up the simulation run. Returns the end state of the simulation."
  (ev/simulation-finished handler current-state))

(defn run [model handler]
  (->> (initialize-simulation-run model handler)
       (running-simulation-timesteps model handler)
       (finish-simulation-run model handler)))
