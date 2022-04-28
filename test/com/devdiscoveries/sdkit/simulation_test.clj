(ns com.devdiscoveries.sdkit.simulation-test
  (:require [midje.sweet :as midje]
            [com.devdiscoveries.sdkit.simulation :as sim]
            [clojure.spec.alpha :as spec]))

(def model {::sim/initial-time 0
            ::sim/timestep 1
            ::sim/final-time 3
            ::sim/name "Test model"})

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
        (sim/get-metadata updated-state ::sim/total-timesteps) => 3)))

(midje/facts "Facts about models."
             (midje/fact "A model has required elements in order to be valid."
                         (let [m {::sim/initial-time 0
                                  ::sim/timestep 1
                                  ::sim/final-time 100
                                  ::sim/name "Test model"}]
                           (spec/conform ::sim/model m) => m )))
