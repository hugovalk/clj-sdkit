(ns com.devdiscoveries.sdkit.simulation-test
  (:require [midje.sweet :as midje]
            [com.devdiscoveries.sdkit.simulation :as sim]))

(midje/fact "A simulation can be run."
      (let [handler (sim/->SimpleStatusHandler (atom nil))]
        (sim/run handler)
        @(:status handler) => ::sim/simulation-finished))
