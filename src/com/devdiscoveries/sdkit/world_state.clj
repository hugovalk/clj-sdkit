(ns com.devdiscoveries.sdkit.world-state
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::timestep number?)
(spec/def ::current-time number?)
(spec/def ::final-time number?)
(spec/def ::state-metadata (spec/keys :req [::timestep
                                            ::final-time
                                            ::current-time]))
(spec/def ::world-state (spec/keys :req [::state-metadata]))

(defn get-metadata [state key]
  (get-in state [::state-metadata key]))

(defn current-time [state]
  (get-metadata state ::current-time))

(defn final-time [state]
  (get-metadata state ::final-time))

(defn timestep [state]
  (get-metadata state ::timestep))

(defn init-from-model [model]
  {::state-metadata {::timestep (bigdec (get-in model [:metadata :timestep]))
                     ::current-time (bigdec (get-in model [:metadata :initial-time]))
                     ::final-time (bigdec (get-in model [:metadata :final-time]))}})

(defn step-time [state]
  (update-in state [::state-metadata ::current-time]
             (fn [t] (+ t (timestep state)))))

(defn save [state k v]
  (if (k state)
    (assoc state k (cons v (k state)))
    (assoc state k (vector v))))

(defn query
  ([state k] (query state k 0))
  ([state k delay]
   (nth (k state) (/ delay (timestep state)) 0.0)))

(defn print-state!
  "Convenience method to print the current world state for debug and logging purposes."
  [state]
  (if (empty? state)
    (println "State is empty.")
    (doseq [[k v]  state] (println k "=" v))))
