(ns com.devdiscoveries.sdkit.sim-test
  (:use [com.devdiscoveries.sdkit.model]))

(def state (atom {}))

(defmodel ref-model
  {}
  (defconst total-population state 10000)
  (defconst advertising-effectiveness state 0.011)
  (defconst contact-rate state 100)
  (defconst adoption-fraction state 0.015)

  (defstock potential-adopters state 10000 (fn [adoption-rate] (- adoption-rate)))
  (defstock adopters state 0.0 (fn [adoption-rate] adoption-rate))

  (defflow adoption-rate state
    (fn [adoption-from-advertising adoption-from-word-of-mouth]
      (+ adoption-from-advertising adoption-from-word-of-mouth)))

  (defconv adoption-from-advertising state
    (fn [potential-adopters advertising-effectiveness]
      (* potential-adopters advertising-effectiveness)))
  (defconv adoption-from-word-of-mouth state
    (fn [contact-rate adoption-fraction potential-adopters adopters total-population]
      (* contact-rate
         adoption-fraction
         potential-adopters
         (/ adopters total-population))))
  )


(defn run-converters []
  (doseq [c (:converters ref-model)] (differential c state)))

(run-converters)


;; run all preparations
;;  - handling sub modules (IGNORE)
;;  - prepare initial values
;;  - prepare values for first time step
;;  - fire simulation initialized event
;;  - execute converters
;;  - fire timestep calculated event

;; loop all time steps
;; Per loop:
;;  - update current time
;;  - prepare values for next time step
;;  - integrate
;;  - execute converters
;;  - fire timestep calculated event

;; finish simulation
;;  - fire simulation finished event

(defn integrate [model state]
  (doseq [s (:stocks model)]
    (let [cur (:current-value s)]
      (reset! cur (+ @cur (* (differential s state)))))))
(run-converters)
(integrate ref-model state)

;(defn evaluate [model state]
;  (->> (set-constants model state)))

;(evaluate my-model state)
