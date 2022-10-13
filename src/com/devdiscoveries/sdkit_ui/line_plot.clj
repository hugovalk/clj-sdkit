(ns com.devdiscoveries.sdkit-ui.line-plot
  (:require [com.devdiscoveries.sdkit.event-handler :as ev]
            [com.devdiscoveries.sdkit.world-state :as state]
            [oz.core :as oz]
            [oz.server :as ozs]
            [clojure.spec.alpha :as spec]))

(defn- html [line-plot]
  [:div
   [:h1 "Live line plot"]
   [:p "Data of the simulation."]
   [:vega-lite line-plot]
   [:p "Should be live updating."]])

(defn- transform-column [from name]
  (map-indexed (fn [i e] {:time i
                          :item name
                          :quantity e}) (reverse from)))

(defn- add-columns-to-data [state columns result]
  (if (empty? columns)
    result
    (let [c (first columns)
          vs (:values result)
          nvs (concat vs (transform-column (get state c) (str c)))]
      (add-columns-to-data state (rest columns) {:values nvs}))))

(defn state-to-plot-data [state columns]
  (add-columns-to-data state columns {:values ()}))

(defn play-data [& names]
  (for [n names
        i (range 20)]
    {:time i :item n :quantity (+ (Math/pow (* i (count n)) 0.8) (rand-int (count n)))}))

(defn- create-line-plot [state columns]
  {:data (state-to-plot-data state columns)
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative"}
              :color {:field "item" :type "nominal"}}
   :mark "line"
   :width "1000"
   :heigth "500"})

(defrecord LinePlotHandler [columns sync-every]
  ev/SimulationEventHandler
  (simulation-initialized [handler initial-state]
    (oz/start-server!)
    (oz/view! (html (create-line-plot initial-state columns))))
  (timestep-calculated [handler updated-state]
    (let [current (state/current-time updated-state)]
      (if (= 0M (mod current sync-every))
        (oz/view! (html (create-line-plot updated-state columns))))))
  (simulation-finished [handler end-state]
    true))
