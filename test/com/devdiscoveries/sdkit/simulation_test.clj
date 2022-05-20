(ns com.devdiscoveries.sdkit.simulation-test
  (:use [com.devdiscoveries.sdkit.sweet])
  (:require [midje.sweet :as midje]
            [com.devdiscoveries.sdkit.event-handler :as ev]
            [com.devdiscoveries.sdkit.simulation :as sim]
            [com.devdiscoveries.sdkit.model :as mod]
            [com.devdiscoveries.sdkit.world-state :as state]
            [clojure.spec.alpha :as spec]))

(defmodel model
  {:initial-time 0
   :timestep 1
   :final-time 3
   :name "Test model"})

(midje/facts "Facts about running simulations."
   (midje/fact "A simulation can be run."
      (let [handler (simple-handler)]
        (sim/run model handler)
        @(:status handler) => ::ev/simulation-finished))

   (midje/fact "Initializing simulation returns initial world state."
      (let [initial-state (sim/initialize-simulation-run model (simple-handler))]
        (spec/conform ::state/world-state initial-state) => initial-state))

   (midje/fact "Running one time step returns valid world state."
      (let [handler (simple-handler)
            initial-state (sim/initialize-simulation-run model handler)
            updated-state (sim/running-simulation-timesteps model handler initial-state)]
        (spec/conform ::state/world-state updated-state) => updated-state))

   (midje/fact "Running simulation performs all time steps."
      (let [handler (simple-handler)
            initial-state (sim/initialize-simulation-run model handler)
            updated-state (sim/running-simulation-timesteps model handler initial-state)]
        (state/total-timesteps initial-state) => 0
        (state/total-timesteps updated-state) => 3))

   (midje/fact "When total time is not divisible by timestep, do not overflow beyond final time"
      (let [handler (simple-handler)]
        (sim/run (update-in model [:metadata :timestep] (fn [a] 2)) handler)
        (state/total-timesteps @(:latest-state handler)) => 2)))


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
         (/ adopters total-population)))))


(midje/facts "Setup initial state tests."
   (let [state (sim/setup-initial-state ref-model)]
     (midje/fact "State is correctly initialized."
                 (count state) => 10)
     (midje/fact "Constants are correctly set."
                 (mod/value contact-rate state) => 100)
     (midje/fact "Stocks are correctly set."
                 (mod/value adopters state) => 0.0)
     (midje/fact "Converters are correctly calculated."
                 (mod/value adoption-from-advertising state) => 110.0)
     (midje/fact "Flows are correctly calculated."
                 (mod/value adoption-rate state) => 110.0)
     (midje/fact "Stock differential is correctly calculated."
                 (mod/differential adopters state) => 110.0)))


(midje/fact :integration "Simulation integration test."
   (let [handler (simple-handler)
         end-state (sim/run ref-model handler)]
     @(:status handler) => :ev/simulation-finished
     (state/total-timesteps end-state) => 100))
