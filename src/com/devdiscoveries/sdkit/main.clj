(ns com.devdiscoveries.sdkit.main
  (:require
   [clojure.tools.logging :refer [info]]))

(defn -main []
  (info "Starting up...")
  (println "Hello World")
  (info "Finished!"))
