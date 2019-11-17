(ns rehook.example
  (:require [rehook.test :as rehook.test]
            [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui]]))

(defui simple-ui
  [_ _ $]
  (let [[x set-x] (rehook/use-state "foo")]
    ($ :div {:onClick   #(set-x "bar")
             :rehook/id :my-div}
       x)))

(defn simple-ui-test []
  (let [scenes (rehook.test/component->scenes {} identity clj->js simple-ui)
        result (rehook.test/play-scenes! scenes 0)]

    (js/console.log
     "first result == foo"
     (= "foo" (-> result :elements :my-div :children first)))

    (let [my-div-on-click (-> result :elements :my-div :args :onClick)]
      (my-div-on-click {})

      (let [result2 (rehook.test/play-scenes! scenes 0)]
        (js/console.log
         "second result == bar"
         (= "bar" (-> result2 :elements :my-div :children first)))))))


(simple-ui-test)