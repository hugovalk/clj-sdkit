(ns com.devdiscoveries.sdkit.simulation-test
  (:require [midje.sweet :as midje]
            [com.devdiscoveries.sdkit.simulation :as sim]
            [clojure.spec.alpha :as spec]))

(defmodel model {::sim/initial-time 0
                 ::sim/timestep 1
                 ::sim/final-time 3
                 ::sim/name "Test model"
                 ::sim/entities [{::sim/type ::sim/constant
                                  ::sim/id :const1
                                  ::sim/value 10}
                                 {::sim/type ::sim/constant
                                  ::sim/id :const2
                                  ::sim/value 20}
                                 {::sim/type ::sim/converter
                                  ::sim/id :conv1
                                  ::sim/formula (+ :const1 :const2)}]})

(defn simple-handler []
  (sim/->SimpleStatusHandler (atom nil) (atom nil)))

(midje/facts "Facts about running simulations."
   (midje/fact "A simulation can be run."
      (let [handler (simple-handler)]
        (sim/run model handler)
        @(:status handler) => ::sim/simulation-finished))

   (midje/fact "Initializing simulation returns initial world state."
      (let [initial-state (sim/initialize-simulation-run model (simple-handler))]
        (spec/conform ::sim/world-state initial-state) => initial-state))

   (midje/fact "Running one time step returns valid world state."
      (let [handler (simple-handler)
            initial-state (sim/initialize-simulation-run model handler)
            updated-state (sim/running-simulation-timesteps model handler initial-state)]
        (spec/conform ::sim/world-state updated-state) => updated-state))

   (midje/fact "Running simulation performs all time steps."
      (let [handler (simple-handler)
            initial-state (sim/initialize-simulation-run model handler)
            updated-state (sim/running-simulation-timesteps model handler initial-state)]
        (sim/get-metadata initial-state ::sim/total-timesteps) => 0
        (sim/get-metadata updated-state ::sim/total-timesteps) => 3))

   (midje/fact "When total time is not divisible by timestep, do not overflow beyond final time"
      (let [handler (simple-handler)]
        (sim/run (assoc model ::sim/timestep 2) handler)
        (sim/get-metadata @(:latest-state handler) ::sim/total-timesteps) => 2)))

(midje/facts "Facts about models."
   (midje/fact "A model has required elements in order to be valid."
      (spec/conform ::sim/model model) => model ))
