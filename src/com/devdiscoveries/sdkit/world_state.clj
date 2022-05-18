(ns com.devdiscoveries.sdkit.world-state
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::total-timesteps number?)
(spec/def ::timesteps-needed number?)
(spec/def ::state-metadata (spec/keys :req [::timesteps-needed
                                            ::total-timesteps]))
(spec/def ::world-state (spec/keys :req [::state-metadata]))

(defn get-metadata [state key]
  (get-in state [::state-metadata key]))

(defn total-timesteps [state]
  (get-metadata state ::total-timesteps))

(defn timesteps-needed [state]
  (get-metadata state ::timesteps-needed))


(defn- calc-timesteps-needed [model]
  (let [meta (:metadata model)
        start (:initial-time meta)
        end (:final-time meta)
        step (:timestep meta)]
    (/ (- end start) step)))

(defn init-from-model [model]
  {::state-metadata {::total-timesteps 0
                     ::timesteps-needed (calc-timesteps-needed model)}})

(defn step-time [state]
  (update-in state [::state-metadata ::total-timesteps] inc))

(defn print-state!
  "Convenience method to print the current world state for debug and logging purposes."
  [state]
  (if (empty? state)
    (println "State is empty.")
    (doseq [[k v]  state] (println k "=" v))))
