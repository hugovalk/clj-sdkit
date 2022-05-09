(ns com.devdiscoveries.sdkit.model
  (:require [clojure.spec.alpha :as spec]
            [clojure.walk :as w]))


(spec/def ::initial-time number?)
(spec/def ::timestep number?)
(spec/def ::final-time number?)
(spec/def ::name string?)

(spec/def ::type #{::constant ::converter})
(spec/def ::id keyword?)
(spec/def ::value number?)
(defmulti entity-type ::type)
(defmethod entity-type ::constant [_]
  (spec/keys :req [::type
                   ::id
                   ::value]))
(defmethod entity-type ::converter [_]
  (spec/keys :req [::type
                   ::id
                   ::formula]))
(spec/def ::entities (spec/multi-spec entity-type ::type))

(spec/def ::model (spec/keys :req [::initial-time
                                   ::final-time
                                   ::timestep
                                   ::name]))


(def my-model {::initial-time 0
               ::final-time 10
               ::timestep 1
               ::name "Test model"
               ::entities [{::id :const1 ::value 10}
                           {::id :const2 ::value 20}
                           {::id :conv1
                            ::args [:const1 :const2]
                            ::formula (fn [c1 c2] (+ c1 c2))}]})

(def state (atom {}))
(defn add-entity [state entity]
  (swap! state (fn [m] (assoc m (id entity) entity))))
(defn print-state! [state]
  (doseq [[k v] @state] (println k "=" (value v state))))

(defprotocol ModelEntity
  (id [self] "Returns key for use in world-state.")
  (value [self state] "Returns the current value.")
  (differential [self state] "Returns the differential to the value."))


(defrecord Constant [const-id internal-value]
  ModelEntity
  (id [self] const-id)
  (value [self state] @internal-value)
  (differential [self state] 0.0))

(defmacro defconst [const-name state value]
  `(do
     (def ~const-name (->Constant ~(keyword const-name) (atom ~value)))
     (add-entity ~state ~const-name)
     ~const-name))
(defconst test-const state 10)
(id test-const)
(value test-const state)
(differential test-const state)


(defrecord Converter [conv-id formula args]
  ModelEntity
  (id [self] conv-id)
  (value [self state] (differential self state))
  (differential [self state]
    (let [eval-args (map (fn [e] (value (e @state) state)) args)]
      (apply formula eval-args))))

(defn char-range [number-needed]
  (into []
        (map (fn [i] (->> i
                          char
                          str
                          symbol))
             (range (int \a) (+ (int \a) number-needed)))))

(defmacro defconv [conv-name state formula]
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~conv-name
         (->Converter ~(keyword conv-name)
                      ~(eval (w/postwalk-replace args-map formula))
                      ~(into [] (map (fn [a] (keyword a)) args))))
       (add-entity ~state ~conv-name)
       ~conv-name)))

(defconv test-conv state (fn [test-const] (+ test-const test-const)))
(id test-conv)
(value test-conv state)
(differential test-conv state)


(defrecord Stock [stock-id current-value formula args]
  ModelEntity
  (id [self] stock-id)
  (value [self state] @current-value)
  (differential [self state]
    (let [eval-args (map (fn [e] (value (e @state) state)) args)]
      (apply formula eval-args))))

(defmacro defstock [stock-name state initial-value formula]
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~stock-name
         (->Stock ~(keyword stock-name)
                  ~(atom initial-value)
                  ~(eval (w/postwalk-replace args-map formula))
                  ~(into [] (map (fn [a] (keyword a)) args))))
       (add-entity ~state ~stock-name)
       ~stock-name)))

(defstock test-stock state 100 (fn [test-const] (+ test-const test-const)))
(id test-stock)
(value test-stock state)
(differential test-stock state)

(defrecord Flow [flow-id formula args]
  ModelEntity
  (id [self] flow-id)
  (value [self state] (differential self state))
  (differential [self state]
    (let [eval-args (map (fn [e] (value (e @state) state)) args)]
      (apply formula eval-args))))

(defmacro defflow [flow-name state formula]
  (let [args (second formula)
        args-map (zipmap args (char-range (count args)))]
    `(do
       (def ~flow-name
         (->Flow ~(keyword flow-name)
                 ~(eval (w/postwalk-replace args-map formula))
                 ~(into [] (map (fn [a] (keyword a)) args))))
       (add-entity ~state ~flow-name)
       ~flow-name)))

(defflow test-flow state (fn [test-const] (+ test-const test-const)))
(id test-flow)
(value test-flow state)
(differential test-flow state)


(defconst total-population state 10000)
(defconst advertising-effectiveness state 0.011)
(defconst contact-rate state 100)
(defconst adoption-fraction state 0.015)

(defstock potential-adopters state 10000 (fn [adoption-rate] (- adoption-rate)))
(defstock adopters state 0.0 (fn [adoption-rate] adoption-rate))

(defconv adoption-from-advertising state
  (fn [potential-adopters advertising-effectiveness]
    (* potential-adopters advertising-effectiveness)))

(defconv adoption-from-word-of-mouth state
  (fn [contact-rate adoption-fraction potential-adopters adopters total-population]
    (* contact-rate
       adoption-fraction
       potential-adopters
       (/ adopters total-population))))

(defflow adoption-rate state
  (fn [adoption-from-advertising adoption-from-word-of-mouth]
    (+ adoption-from-advertising adoption-from-word-of-mouth)))


;(defmodel my-model []
;  (defconst total-population 10000))

(def consts [total-population advertising-effectiveness contact-rate adoption-fraction])
(def stocks [potential-adopters adopters])
(def convs [adoption-from-advertising adoption-from-word-of-mouth])
(def flows [adoption-rate])

(defn run-converters []
  (doseq [c convs] (differential c state)))
(run-converters)

;; initialized

(defn integrate [state]
  (doseq [s stocks]
    (let [cur (:current-value s)]
      (reset! cur (+ @cur (* (differential s state)))))))
(run-converters)
(integrate state)

;(defn evaluate [model state]
;  (->> (set-constants model state)))

;(evaluate my-model state)
