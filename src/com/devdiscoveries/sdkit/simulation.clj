(ns com.devdiscoveries.sdkit.simulation
  (:require [clojure.tools.logging :refer [info]]
            [clojure.spec.alpha :as spec]))

(spec/def ::initial-time number?)
(spec/def ::time-step number?)
(spec/def ::final-time number?)
(spec/def ::name string?)
(spec/def ::model (spec/keys :req [::initial-time
                                   ::final-time
                                   ::time-step
                                   ::name]))

(spec/def ::total-time-steps number?)
(spec/def ::time-steps-needed number?)
(spec/def ::state-metadata (spec/keys :req [::time-steps-needed
                                            ::total-time-steps]))
(spec/def ::world-state (spec/keys :req [::state-metadata]))

(defn timesteps-needed [model]
  (let [start (::initial-time model)
        end (::final-time model)
        step (::time-step model)]
    (/ (- end start) step)))

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
  (let [initial-state {::state-metadata {::total-time-steps 0
                                         ::time-steps-needed (timesteps-needed model)}}]
    (simulation-initialized handler initial-state)
    initial-state))

(defn running-simulation-time-steps [model handler current-state]
  "Running the next time-step of the model. Returns the updated state of the simulation."
  (info current-state)
  (if (<= (get-in current-state [::state-metadata ::time-steps-needed])
         (get-in current-state [::state-metadata ::total-time-steps]))
    current-state
    (let [updated-state (update-in current-state
                                   [::state-metadata ::total-time-steps]
                                   inc)]
      (timestep-calculated handler updated-state)
      (recur model handler updated-state))))

(defn finish-simulation-run [model handler current-state]
  "Finishing up the simulation run. Returns the end state of the simulation."
  (simulation-finished handler current-state))

(defn run [model handler]
  (->> (initialize-simulation-run model handler)
       (running-simulation-time-steps model handler)
       (finish-simulation-run model handler)))