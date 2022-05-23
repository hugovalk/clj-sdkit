(ns com.devdiscoveries.sdkit.world-state-test
  (:require [com.devdiscoveries.sdkit.world-state :as s]
            [midje.sweet :as midje]))


(midje/facts "Storing values in state and retrieving them."
   (let [state {::s/state-metadata {::s/timestep 0.25
                                    ::s/timesteps-needed 400
                                    ::s/total-timesteps 20}}]
     (midje/fact "Storing a value in the state."
        (let [res (s/save state :test 1)]
          (s/query res :test) => 1))
     (midje/fact "Storing values occurs in the right order."
        (let [r1 (s/save state :test 1)
              r2 (s/save r1 :test 2)
              res (s/save r2 :test 3)]
          (:test res) => '(3 2 1)))
     (midje/fact "Storing and retrieving a value with a delay"
        (let [res (assoc state :test '(12 11 10 9 8 7 6 5 4 3 2 1))]
          (s/query res :test 1) => 8))))
