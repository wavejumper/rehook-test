(ns rehook.example
  (:require [rehook.test :as rehook.test]
            [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui]]))

(defui simple-ui
  [_ _ $]
  (js/console.log "I am being invoked...")
  (let [[x set-x] (rehook/use-state "foo")]

    ^{:rehook/id "test-div"}
    ($ :div {:onClick #(set-x "bar")}
       x)))

(defn simple-ui-test []
  (rehook.test/component->scenes {} identity clj->js my-cool-ui)

  )