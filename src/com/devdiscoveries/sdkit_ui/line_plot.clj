(ns com.devdiscoveries.sdkit-ui.line-plot
  (:require [com.devdiscoveries.sdkit.event-handler :as ev]
            [com.devdiscoveries.sdkit.world-state :as state]
            [oz.core :as oz]
            [oz.server :as ozs]
            [clojure.spec.alpha :as spec]))

(defn- html [state line-plot]
  [:div
   [:h1 (state/name state)]
   [:div
    [:table
     [:tr [:td "start time:"] [:td (double (state/initial-time state))]]
     [:tr [:td "current time:"] [:td (double (state/current-time state))]]
     [:tr [:td "final time:"] [:td (double (state/final-time state))]]
     [:tr [:td "timestep:"] [:td (double (state/timestep state))]]]]
   [:p "Data of the simulation."]
   [:vega-lite line-plot]
   [:p "Should be live updating."]])

(defn- transform-column [state from name]
  (map-indexed (fn [i e] {:time (double (+ (state/initial-time state) (* i (state/timestep state))))
                          :item name
                          :quantity (double e)}) (reverse from)))

(defn- add-columns-to-data [state columns result]
  (if (empty? columns)
    result
    (let [c (first columns)
          vs (:values result)
          nvs (concat vs (transform-column state (get state c) (str c)))]
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
    (if (not (ozs/web-server-started?))
      (oz/start-server!))
    (oz/view! (html initial-state (create-line-plot initial-state columns))))
  (timestep-calculated [handler updated-state]
    (let [current (state/current-time updated-state)]
      (if (= 0M (mod current sync-every))
        (oz/view! (html updated-state (create-line-plot updated-state columns))))))
  (simulation-finished [handler end-state]
    true))
