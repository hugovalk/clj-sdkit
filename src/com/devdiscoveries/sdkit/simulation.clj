(ns com.devdiscoveries.sdkit.simulation
  (:require [clojure.tools.logging :refer [info]]
            [clojure.spec.alpha :as spec]))

(spec/def ::total-timesteps number?)
(spec/def ::timesteps-needed number?)
(spec/def ::state-metadata (spec/keys :req [::timesteps-needed
                                            ::total-timesteps]))
(spec/def ::world-state (spec/keys :req [::state-metadata]))

(defn timesteps-needed [model]
  (let [start (::initial-time model)
        end (::final-time model)
        step (::timestep model)]
    (/ (- end start) step)))

(defn get-metadata [state key]
  (get-in state [::state-metadata key]))

(defprotocol SimulationEventHandler
  "Protocol for Event handler that handles events from a simulation run."
  (simulation-initialized [handler initial-state] "Called when the simulation is initialized.")
  (timestep-calculated [handler updated-state] "Called when a time step has been calculated.")
  (simulation-finished [handler end-state] "Called when the simulation is finished."))

(defrecord SimpleStatusHandler [status latest-state]
  SimulationEventHandler
  (simulation-initialized [handler initial-state]
    (info "Simulation initialized...")
    (reset! latest-state initial-state)
    (reset! status ::simulation-initialized))
  (timestep-calculated [handler updated-state]
    (info "Time step calculated: ")
    (reset! latest-state updated-state)
    (reset! status ::timestep-calculated))
  (simulation-finished [handler end-state]
    (info "Simulation finished!")
    (reset! latest-state end-state)
    (reset! status ::simulation-finished)))

(defn initialize-simulation-run [model handler]
  "Initializing the simulation run. Returns an atom, containing the initial state of the simulation."
  (let [initial-state {::state-metadata {::total-timesteps 0
                                         ::timesteps-needed (timesteps-needed model)}}]
    (simulation-initialized handler initial-state)
    initial-state))

(defn running-simulation-timesteps [model handler current-state]
  "Running the next timestep of the model. Returns the updated state of the simulation."
  (info current-state)
  (if (> (get-metadata current-state ::timesteps-needed)
          (get-metadata current-state ::total-timesteps))
    (let [updated-state (update-in current-state
                                   [::state-metadata ::total-timesteps]
                                   inc)]
      (timestep-calculated handler updated-state)
      (recur model handler updated-state))
    current-state))

(defn finish-simulation-run [model handler current-state]
  "Finishing up the simulation run. Returns the end state of the simulation."
  (simulation-finished handler current-state))

(defn run [model handler]
  (->> (initialize-simulation-run model handler)
       (running-simulation-timesteps model handler)
       (finish-simulation-run model handler)))
