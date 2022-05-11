(ns com.devdiscoveries.sdkit.model-test
  (:require [com.devdiscoveries.sdkit.model :as mod]
            [midje.sweet :as midje]
            [clojure.spec.alpha :as spec]))

(def state (atom {}))

(midje/facts "Facts about the defconst macro."
   (let [state (atom {})
         const (mod/defconst test-const state 12)]
     (midje/fact "A constant has an id that is the keyword of the symbol given."
                 (mod/id const) => :test-const)
     (midje/fact "A constant has its value correctly set."
                 (mod/value const state) => 12)
     (midje/fact "A constant returns 0.0 as differential."
                 (mod/differential const state) => 0.0)
     (midje/fact "A constant is added to the state."
                 (:test-const @state) => const)))


(midje/facts "Facts about the defconv macro."
   (let [state (atom {})
         const (mod/defconst test-const state 12)
         conv (mod/defconv test-conv state (fn [test-const] (+ test-const test-const)))]
     (midje/fact "A converter has an id."
                 (mod/id conv) => :test-conv)
     (midje/fact "A converter's value is calculated from its formula."
                 (mod/value conv state) => 24)
     (midje/fact "A converter's differential is nil as it has no meaning."
                 (mod/differential conv state) => nil)
     (midje/fact "A converter is added to the state."
                 (:test-conv @state) => conv)))

(midje/facts "Facts about the defstock macro."
   (let [state (atom {})
         const (mod/defconst test-const state 12)
         stock (mod/defstock test-stock state 100 (fn [test-const] (+ 2 test-const)))]
     (midje/fact "A stock has an id."
                 (mod/id stock) => :test-stock)
     (midje/fact "A stock's value is set to the initial value."
                 (mod/value stock state) => 100)
     (midje/fact "A stock's differential is calculated from the formula."
                 (mod/differential stock state) => 14)
     (midje/fact "A stock is added to the state."
                 (:test-stock @state) => stock)))

(midje/facts "Facts about the defflow macro."
   (let [state (atom {})
         const (mod/defconst test-const state 12)
         flow (mod/defflow test-flow state (fn [test-const] (+ test-const test-const)))]
     (midje/fact "A flow has an id."
                 (mod/id flow) => :test-flow)
     (midje/fact "A converter's value is calculated from its formula."
                 (mod/value flow state) => 24)
     (midje/fact "A converter's differential is nil as it has no meaning."
                 (mod/differential flow state) => nil)
     (midje/fact "A converter is added to the state."
                 (:test-flow @state) => flow)))



(midje/facts "Facts about models."
   (let [state (atom {})
         model (mod/defmodel test-model
                 {:initial-time 0
                  :timestep 1
                  :final-time 3
                  :name "Test model"}
                 (mod/defconst const state 1)
                 (mod/defconv conv state (fn [const] (+ 2 const)))
                 (mod/defstock stock state 10 (fn [const] (+ 2 const)))
                 (mod/defflow flow state (fn [const] (+ 2 const))))]
     (midje/fact "A model has all its required parts."
                 (spec/conform ::mod/model model) => model)
     (midje/fact "The constants are set."
                 (first (:constants model)) => const)
     (midje/fact "The stocks are set."
                 (first (:stocks model)) => stock)
     (midje/fact "The converters are set."
                 (first (:converters model)) => conv)
     (midje/fact "The flows are set."
                 (first (:flows model)) => flow)))
