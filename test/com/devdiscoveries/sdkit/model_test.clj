(ns com.devdiscoveries.sdkit.model-test
  (:require [com.devdiscoveries.sdkit.model :as mod]
            [midje.sweet :as midje]
            [clojure.spec.alpha :as spec]))

(midje/facts "Facts about the defconst macro."
   (let [const (mod/defconst test-const 12)]
     (midje/fact "A constant has an id that is the keyword of the symbol given."
                 (mod/id const) => :test-const)
     (midje/fact "A constant has its default-value correctly set."
                 (:default-value const) => 12)))

(midje/facts "Facts about the defconv macro."
   (let [state (atom {:test-const 12})
         const (mod/defconst test-const 12)
         conv (mod/defconv test-conv (fn [test-const] (+ test-const test-const)))]
     (midje/fact "A converter has an id."
                 (mod/id conv) => :test-conv)))

(midje/facts "Facts about the defstock macro."
   (let [const (mod/defconst test-const 12)
         stock (mod/defstock test-stock 100 (fn [test-const] (+ 2 test-const)))]
     (midje/fact "A stock has an id."
                 (mod/id stock) => :test-stock)
     (midje/fact "A stock's default-value is set."
                 (:default-value stock) => 100)))

(midje/facts "Facts about the defflow macro."
   (let [const (mod/defconst test-const 12)
         flow (mod/defflow test-flow (fn [test-const] (+ test-const test-const)))]
     (midje/fact "A flow has an id."
                 (mod/id flow) => :test-flow)))

(midje/facts "Facts about models."
   (let [model (mod/defmodel test-model
                 {:initial-time 0
                  :timestep 1
                  :final-time 3
                  :name "Test model"}
                 (mod/defconst const 1)
                 (mod/defconv conv (fn [const] (+ 2 const)))
                 (mod/defstock stock 10 (fn [const] (+ 2 const)))
                 (mod/defflow flow (fn [const] (+ 2 const))))]
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

