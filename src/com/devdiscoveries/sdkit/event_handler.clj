(ns com.devdiscoveries.sdkit.event-handler
  (:require [clojure.tools.logging :refer [info]]))

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
