(ns autotest
  (:require [midje.repl :as m]))

(defn -main []
  (m/autotest :filter (complement :integration) :dirs "test" "src"))
