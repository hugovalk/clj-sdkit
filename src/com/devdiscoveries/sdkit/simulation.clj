(ns com.devdiscoveries.sdkit.simulation
  (:require [com.devdiscoveries.sdkit.model :as mod]
            [com.devdiscoveries.sdkit.event-handler :as ev]
            [com.devdiscoveries.sdkit.world-state :as s]
            [clojure.tools.logging :refer [info]]))

(defn- add-constants [state constants]
  (if (and constants (not (empty? constants)))
    (let [c (first constants)]
      (recur (s/save state (mod/id c) (:default-value c)) (rest constants)))
    state))

(defn- add-stocks [state last-state stocks integrator-fn]
  (if (and stocks (not (empty? stocks)))
    (let [s (first stocks)
          value (if last-state
                  (integrator-fn last-state s)
                  (:default-value s))]
      (recur (s/save state (mod/id s) value)
             last-state
             (rest stocks)
             integrator-fn))
    state))

(defn- add-converters [state converters]
  (if (and converters (not (empty? converters)))
    (let [c (first converters)
          value (mod/value c state)]
      (recur (s/save state (mod/id c) value)
             (rest converters)))
    state))

(defn- add-flows [state flows]
  (if (and flows (not (empty? flows)))
    (let [f (first flows)
          value (mod/value f state)]
      (recur (s/save state (mod/id f) value)
             (rest flows)))
    state))

(defn euler-integrator [state stock]
  (let [sid (mod/id stock)
        dt (s/timestep state)]
    (+ (s/query state sid)
       (* dt (mod/differential stock state)))))

(defn setup-initial-state [model]
  (-> (s/init-from-model model)
      (add-constants (:constants model))
      (add-stocks nil (:stocks model) nil)
      (add-converters (:converters model))
      (add-flows (:flows model))))

(defn calculate-new-state [state model integrator-fn]
  (-> (s/step-time state)
      (add-constants (:constants model))
      (add-stocks state (:stocks model) integrator-fn)
      (add-converters (:converters model))
      (add-flows (:flows model))))


(defn initialize-simulation-run [model handler]
  "Initializing the simulation run. Returns an atom, containing the initial state of the simulation."
  (let [initial-state (setup-initial-state model)]
    (ev/simulation-initialized handler initial-state)
    initial-state))

(defn running-simulation-timesteps [model handler current-state]
  "Running the next timestep of the model. Returns the updated state of the simulation."
  (if (>= (s/final-time current-state)
          (+ (s/current-time current-state) (s/timestep current-state)))
    (let [updated-state (calculate-new-state current-state model euler-integrator)]
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
