(ns com.devdiscoveries.sdkit.sweet
  (:require [com.devdiscoveries.sdkit.model :as model]
            [com.devdiscoveries.sdkit.simulation :as sim]
            [com.devdiscoveries.sdkit.event-handler :as ev]
            [clojure.walk :as w]
            [clojure.string :as s]))


(defn- symbol-range
  "Helper function that returns a vector of symbols, starting with 'a', 'b', etc."
  [chars-needed]
  (into []
        (map (fn [i] (->> i
                          char
                          str
                          symbol))
             (range (int \a) (+ (int \a) chars-needed)))))


(defmacro defconst [const-name default-value]
  "Macro that makes defining a Constant and adding it to the world state easier."
  `(do
     (def ~const-name (model/->Constant ~(keyword const-name) ~default-value))
     ~const-name))


(defmacro defconv [conv-name formula]
  "Macro that makes defining a Converter and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Converter. "
  (let [args (second formula)
        args-map (zipmap args (symbol-range (count args)))]
    `(do
       (def ~conv-name
         (model/->Converter ~(keyword conv-name)
                      ~(eval (w/postwalk-replace args-map formula))
                      ~(into [] (map (fn [a] (keyword a)) args))))
       ~conv-name)))


(defmacro defstock [stock-name default-value formula]
    "Macro that makes defining a Stock and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Stock. "
  (let [args (second formula)
        args-map (zipmap args (symbol-range (count args)))]
    `(do
       (def ~stock-name
         (model/->Stock ~(keyword stock-name)
                  ~default-value
                  ~(eval (w/postwalk-replace args-map formula))
                  ~(into [] (map (fn [a] (keyword a)) args))))
       ~stock-name)))


(defmacro defflow
  "Macro that makes defining a Flow and adding it to the world state easier. The formula has to given in the form of (fn [arg1 arg2] ...body...). The args must refer to other model entities, as they will be converted to keywords to be used in the constructor for Flow. "
  ([flow-name formula] `(defflow ~flow-name ~formula 0.0))
  ([flow-name formula delay]
  (let [args (second formula)
        args-map (zipmap args (symbol-range (count args)))]
    `(do
       (def ~flow-name
         (model/->Flow ~(keyword flow-name)
                       ~(eval (w/postwalk-replace args-map formula))
                       ~(into [] (map (fn [a] (keyword a)) args))
                       ~delay))
       ~flow-name))))


(defn- filter-model-entities [macro coll]
    (into [] (filter (fn [e] (s/includes? (first e) (str macro))) coll)))

(defmacro defmodel [model-name model-metadata & body]
  (let [stocks (filter-model-entities 'defstock body)
        flows (filter-model-entities 'defflow body)
        converters (filter-model-entities 'defconv body)
        consts (filter-model-entities 'defconst body)]
    `(do
       (def ~model-name (model/->Model ~model-metadata ~stocks ~flows ~converters ~consts))
       ~model-name)))

(defn csv-handler [file-path column-ks]
  (ev/->CSVHandler file-path column-ks))

(defn simple-handler []
  (ev/->SimpleStatusHandler (atom nil) (atom nil)))

(defn logging-handler
  ([every] (logging-handler every false))
  ([every log-state?]
   (ev/->LoggingStatusHandler (atom nil) every log-state?)))

(defn run-simulation [model & handlers]
  (sim/run model handlers))
