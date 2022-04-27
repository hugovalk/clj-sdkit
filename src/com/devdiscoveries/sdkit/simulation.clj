(ns com.devdiscoveries.sdkit.simulation)

(defprotocol SimulationEventHandler
  "Protocol for Event handler that handles events from a simulation run."
  (simulation-initialized [handler] "Called when the simulation is initialized.")
  (timestep-calculated [handler] "Called when a time step has been calculated.")
  (simulation-finished [handler] "Called when the simulation is finished."))

(defrecord SimpleStatusHandler [status]
  SimulationEventHandler
  (simulation-initialized [handler]
    (reset! status ::simulation-initialized))
  (timestep-calculated [handler]
    (reset! status ::timestep-calculated))
  (simulation-finished [handler]
    (reset! status ::simulation-finished)))

(defn run [handler]
  (simulation-initialized handler)
  (timestep-calculated handler)
  (simulation-finished handler))
