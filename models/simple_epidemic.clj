(ns simple-epidemic
  (:use [com.devdiscoveries.sdkit.sweet]))

;; Simple model with absolute immunity after being recovered.
(defmodel simple-ep-model
  {:initial-time 0
   :final-time 100
   :timestep 0.2
   :name "Simple epidemic model"}
  (defconst population-size 1000000)
  (defconst infection-likelyhood 0.5)
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


;; Slightly more complicated model, where after being recovered, there is a chance to become infected again.
(defmodel ep-model-2
  {:initial-time 0
   :final-time 400
   :timestep 0.2
   :name "Simple epidemic model"}
  (defconst population-size 1000000)
  (defconst infection-likelyhood 0.4)
  (defconst recovery-rate 0.01)
  (defconst reinfection-likelyhood 0.1)
  (defstock susceptible 999999 (fn [infection-transmitted] (- infection-transmitted)))
  (defstock infected 1
    (fn [infection-transmitted people-recovered reinfection-transmitted]
      (- (+ infection-transmitted reinfection-transmitted) people-recovered)))
  (defstock recovered 0
    (fn [people-recovered recovered-becoming-susceptible] (- people-recovered recovered-becoming-susceptible)))
  (defstock recovered-susceptible 0
    (fn [recovered-becoming-susceptible reinfection-transmitted] (- recovered-becoming-susceptible reinfection-transmitted)))
  (defflow infection-transmitted
    (fn [infection-likelyhood infected susceptible population-size]
      (/ (* infection-likelyhood infected susceptible)
         population-size)))
  (defflow people-recovered
    (fn [infected recovery-rate]
      (* infected recovery-rate)))
  (defflow recovered-becoming-susceptible
    (fn [people-recovered] people-recovered) 50)
  (defflow reinfection-transmitted
    (fn [infection-likelyhood infected recovered-susceptible population-size]
      (/ (* infection-likelyhood infected recovered-susceptible)
         population-size))))


(def handler2 (csv-handler "/tmp/epidemic2.csv" [:susceptible :infected :recovered :recovered-susceptible :infection-transmitted :people-recovered :recovered-becoming-susceptible :reinfection-transmitted]))
(run-simulation ep-model-2 handler2)
