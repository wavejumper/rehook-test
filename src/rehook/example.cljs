(ns rehook.example
  (:require [rehook.test :as rehook.test]
            [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui]]))

(defui my-cool-ui
  [_ _ $]
  (js/console.log "I am being invoked...")
  (let [[x set-x] (rehook/use-state "My state")]
    ($ :div {:onClick #(set-x "y")}
       x)))

(js/console.log my-cool-ui )

(defn a-test []
  (rehook.test/component->scenes {} identity clj->js my-cool-ui))

(a-test)