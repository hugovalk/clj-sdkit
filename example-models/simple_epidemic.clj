(ns simple-epidemic
  (:use [com.devdiscoveries.sdkit.sweet]
        [com.devdiscoveries.sdkit-ui.sweet]))

;; Simple epidemic model, where after being recovered, there is a chance to become infected again.
(defmodel ep-model
  {:initial-time 0
   :final-time 400
   :timestep 0.2
   :name "Simple epidemic model"}
  (defconst population-size 1000000)
  (defconst infection-likelyhood 0.4)
  (defconst recovery-rate 0.05)
  (defconst reinfection-likelyhood 0.2)
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
    (fn [reinfection-likelyhood infected recovered-susceptible population-size]
      (/ (* reinfection-likelyhood infected recovered-susceptible)
         population-size))))


(def handler (csv-handler "/tmp/epidemic.csv" [:susceptible :infected :recovered :recovered-susceptible :infection-transmitted :people-recovered :recovered-becoming-susceptible :reinfection-transmitted]))

(def plothandler (line-plot-handler [:susceptible :infected :recovered :recovered-susceptible] 5))

(run-simulation ep-model plothandler (logging-handler 10 false))
