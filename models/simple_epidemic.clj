(ns simple-epidemic
  (:use [com.devdiscoveries.sdkit.sweet]))

(defmodel simple-ep-model
  {:initial-time 0
   :final-time 100
   :timestep 0.1
   :name "Simple epidemic model"}
  (defconst population-size 1000000)
  (defconst infection-likelyhood 0.3)
  (defconst recovery-rate 0.1)
  (defstock susceptible 999999 (fn [infection-transmitted] (- infection-transmitted)))
  (defstock infected 1
    (fn [infection-transmitted people-recovered] (- infection-transmitted people-recovered)))
  (defstock recovered 0 (fn [people-recovered] people-recovered))
  (defflow infection-transmitted
    (fn [infection-likelyhood infected susceptible population-size]
      (/ (* infection-likelyhood infected susceptible)
         population-size)))
  (defflow people-recovered
    (fn [infected recovery-rate]
      (* infected recovery-rate))))

(def handler (csv-handler "/tmp/simple-epidemic.csv" [:susceptible :infected :recovered]))
(run-simulation simple-ep-model handler)
