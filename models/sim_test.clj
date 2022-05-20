(ns sim-test
  (:use [com.devdiscoveries.sdkit.sweet]))

(def state (atom {}))

(defmodel ref-model
  {:initial-time 0
   :final-time 10
   :timestep 0.25
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

(def handler (csv-handler "/tmp/output.csv" [:potential-adopters :adopters :adoption-rate]))
(run-simulation ref-model handler)
