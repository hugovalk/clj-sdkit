(ns com.devdiscoveries.sdkit-ui.sweet
  (:require [com.devdiscoveries.sdkit-ui.line-plot :as lp]))

(defn line-plot-handler [columns sync-every]
  (lp/->LinePlotHandler columns sync-every))
