(ns com.devdiscoveries.sdkit-ui.line-plot-test
  (:require [com.devdiscoveries.sdkit-ui.line-plot :as lp]
            [com.devdiscoveries.sdkit.world-state :as s]
            [midje.sweet :as midje]))


(midje/facts "Calculating line plot data from world state."
   (let [state {::s/state-metadata {::s/timestep 0.25
                                    ::s/timesteps-needed 400
                                    ::s/total-timesteps 20}}]
     (midje/fact "Field from state ends up in line plot data."
                 (let [r1 (s/save state :test 1)
                       r2 (s/save r1 :test 2)
                       r3 (s/save r2 :test 3)
                       r4 (s/save r3 :test2 5)
                       r5 (s/save r4 :test2 2)
                       init-state (s/save r5 :test2 7)
                       res (lp/state-to-plot-data init-state [:test :test2])]
                   (count (:values res)) => 6))))
