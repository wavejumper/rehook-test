(ns rehook.demo.app
  (:require [rehook.test :as rehook.test :refer-macros [with-component-mounted defuitest is io]]
            [rehook.test.browser :as test.browser]
            [rehook.dom :refer-macros [defui ui]]
            [rehook.dom.browser :as dom.browser]
            ["react-dom" :as react-dom]
            [rehook.demo.todo :as todo]))

(defn todo-test []
  (let [scenes (rehook.test/init (todo/system) identity clj->js todo/todo-app)]
    (with-component-mounted [scene1 (rehook.test/mount! scenes)]
      (is scene1 "Initial render should show 4 TODO items" true)
      (swap! scenes update :tests conj
             {:item "Initial render should show 4 TODO items"
              :pass true
              :type :assertion
              :scene 0
              :sexp '(= 4 (count state))})

      (swap! scenes update :tests conj
             {:mutation [:invoke-prop :clear-completed :onClick]
              :type     :mutation
              :pass     true
              :scene    0})

      (swap! scenes update :tests conj
             {:item "After clicking 'Clear Completed' it should clear TODO items"
              :pass true
              :scene 1
              :type  :assertion
              :sexp '(= 0 (count state))})

      (rehook.test/invoke-prop scene1 :clear-completed :onClick {})

      (with-component-mounted [_ (rehook.test/mount! scenes scene1)]
        @scenes))))

(defui heading [_ _]
  [:h1 {}
   [:a {:href   "https://github.com/wavejumper/rehook"
        :target "_blank"}
    "rehook"]])

(defui rehook-test-container [_ _]
  [:div {:style {:width       "calc(100% - 128px)"
                 :maxMidth    "680px"
                 :marginLeft  "64px"
                 :marginRight "64px"
                 :fontFamily  "'Open Sans', sans-serif"
                 :lineHeight  "1.5"
                 :color       "#24292e"}}
   [heading]
   [:h2 {} "Demo"]
   [:p {} "The demo below is the output of the unit tests written for rehook's own todomvc."]
   [:p {} "You can view the source code here."]
   [:p {} "You can see the tests running in a headless CI environment here."]
   [test.browser/testcard]])

(defn ^:dev/after-load render! []
  (let [scenes (todo-test)]
    (js/console.log "rendering rehook.test ~~~ ♪┏(・o･)┛♪")
    (react-dom/render
     (dom.browser/bootstrap {:scenes scenes} identity clj->js rehook-test-container)
     (js/document.getElementById "app"))))

(defn main []
  (render!))