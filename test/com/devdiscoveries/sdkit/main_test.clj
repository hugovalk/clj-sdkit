(ns com.devdiscoveries.sdkit.main-test
  (:require [com.devdiscoveries.sdkit.main :as sut]
            [midje.sweet :as midje]))

(midje/fact "Test that Midje is actually working."
      true => true)
