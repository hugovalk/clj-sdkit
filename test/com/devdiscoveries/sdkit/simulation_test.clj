(ns com.devdiscoveries.sdkit.simulation-test
  (:require [midje.sweet :as midje]
            [com.devdiscoveries.sdkit.simulation :as sim]
            [clojure.spec.alpha :as spec]))

(def model {::sim/initial-time 0
            ::sim/time-step 1
            ::sim/final-time 100
            ::sim/name "Test model"})

(midje/fact "A simulation can be run."
      (let [handler (sim/->SimpleStatusHandler (atom nil))]
        (sim/run model handler)
        @(:status handler) => ::sim/simulation-finished))

(midje/facts "Facts about models."
             (midje/fact "Validating a model works."
                         (let [m {::sim/initial-time 0
                                  ::sim/time-step 1
                                  ::sim/final-time 100
                                  ::sim/name "Test model"}]
                           (spec/conform ::sim/model m) => m )))
