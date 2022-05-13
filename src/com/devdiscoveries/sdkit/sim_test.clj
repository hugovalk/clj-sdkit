(ns com.devdiscoveries.sdkit.sim-test
  (:use [com.devdiscoveries.sdkit.model]))

(def state (atom {}))

(defmodel ref-model
  {:initial-time 0
   :final-time 100
   :timestep 1
   :name "ref-model"}
  (defconst total-population 10000)
  (defconst advertising-effectiveness 0.011)
  (defconst contact-rate 100)
  (defconst adoption-fraction 0.015)

  (defstock potential-adopters 10000 (fn [adoption-rate] (- adoption-rate)))
  (defstock adopters 0.0 (fn [adoption-rate] adoption-rate))

  (defflow adoption-rate
    (fn [adoption-from-advertising adoption-from-word-of-mouth]
      (+ adoption-from-advertising adoption-from-word-of-mouth)))

  (defconv adoption-from-advertising
    (fn [potential-adopters advertising-effectiveness]
      (* potential-adopters advertising-effectiveness)))
  (defconv adoption-from-word-of-mouth
    (fn [contact-rate adoption-fraction potential-adopters adopters total-population]
      (* contact-rate
         adoption-fraction
         potential-adopters
         (/ adopters total-population))))
  )


;; run all preparations
;;  - IGNORE - handling sub modules
;;  - DONE all entities are already initialized - prepare initial values
;;  - WIP this means current time state is correctly setup - prepare values for first time step
;;  - TODO fire simulation initialized event
;;  - TODO execute converters
;;  - TODO fire timestep calculated event

;; loop all time steps
;; Per loop:
;;  - update current time
;;  - prepare values for next time step
;;  - integrate
;;  - execute converters
;;  - fire timestep calculated event

;; finish simulation
;;  - fire simulation finished event

;(defn integrate [model state]
;  (doseq [s (:stocks model)]
;    (let [cur (:current-value s)]
;      (reset! cur (+ @cur (* (differential s state)))))))
;(run-converters)
;(integrate ref-model state)

;(defn evaluate [model state]
;  (->> (set-constants model state)))

;(evaluate my-model state)
