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

(defprotocol SimulationEventHandler
  "Protocol for Event handler that handles events from a simulation run."
  (simulation-initialized [handler] "Called when the simulation is initialized.")
  (timestep-calculated [handler] "Called when a time step has been calculated.")
  (simulation-finished [handler] "Called when the simulation is finished."))

(defrecord SimpleStatusHandler [status]
  SimulationEventHandler
  (simulation-initialized [handler]
    (info "Simulation initialized...")
    (reset! status ::simulation-initialized))
  (timestep-calculated [handler]
    (info "Time step calculated: ")
    (reset! status ::timestep-calculated))
  (simulation-finished [handler]
    (info "Simulation finished!")
    (reset! status ::simulation-finished)))

(defn run [model handler]
  (simulation-initialized handler)
  (timestep-calculated handler)
  (simulation-finished handler))
