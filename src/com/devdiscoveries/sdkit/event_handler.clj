(ns com.devdiscoveries.sdkit.event-handler
  (:require [clojure.tools.logging :refer [info]]
            [clojure.string :refer [join]]
            [com.devdiscoveries.sdkit.world-state :as state]))

(defprotocol SimulationEventHandler
  "Protocol for Event handler that handles events from a simulation run."
  (simulation-initialized [handler initial-state] "Called when the simulation is initialized.")
  (timestep-calculated [handler updated-state] "Called when a time step has been calculated.")
  (simulation-finished [handler end-state] "Called when the simulation is finished."))

(defrecord SimpleStatusHandler [status latest-state]
  SimulationEventHandler
  (simulation-initialized [handler initial-state]
    (reset! latest-state initial-state)
    (reset! status ::simulation-initialized))
  (timestep-calculated [handler updated-state]
    (reset! latest-state updated-state)
    (reset! status ::timestep-calculated))
  (simulation-finished [handler end-state]
    (reset! latest-state end-state)
    (reset! status ::simulation-finished)))


(defn log-state!
  "Convenience method to log the current world state for debug and logging purposes."
  [state]
  (if (empty? state)
    (info "State is empty.")
    (info (clojure.string/join "\n" (map (fn [[k v]] (str k " = " v)) state)))))

(defrecord LoggingStatusHandler [status log-every]
  SimulationEventHandler
  (simulation-initialized [handler initial-state]
    (info "Simulation initialized...")
    (info "Initial state:")
    (log-state! initial-state)
    (reset! status ::simulation-initialized))
  (timestep-calculated [handler updated-state]
    (let [current (state/current-time updated-state)]
      (if (= 0 (mod current log-every))
        (do
          (info "Time step" current "calculated: ")
          (log-state! updated-state)))
      (reset! status ::timestep-calculated)))
  (simulation-finished [handler end-state]
    (info "Simulation finished!")
    (reset! status ::simulation-finished)))

(defrecord CSVHandler [file-name column-keys]
  SimulationEventHandler
  (simulation-initialized [handler initial-state]
    (spit file-name "timestep,")
    (spit file-name (join "," (map name column-keys)) :append true)
    (spit file-name "\n" :append true)
    (spit file-name (str (state/current-time initial-state) ",") :append true)
    (spit file-name (join "," (map (fn [k] (state/query initial-state k)) column-keys)) :append true)
    (spit file-name "\n" :append true))
  (timestep-calculated [handler updated-state]
    (spit file-name (str (state/current-time updated-state) ",") :append true)
    (spit file-name (join "," (map (fn [k] (state/query updated-state k)) column-keys)) :append true)
    (spit file-name "\n" :append true))
  (simulation-finished [handler end-state]))
