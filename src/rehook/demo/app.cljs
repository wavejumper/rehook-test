(ns rehook.demo.app
  (:require [rehook.test :as rehook.test :refer-macros [with-component-mounted defuitest is io]]
            [rehook.test.browser :as test.browser]
            [rehook.dom :refer-macros [defui ui]]
            [rehook.demo.todo :as todo]))

(def todo-app
  {:system    todo/system
   :ctx-f     identity
   :props-f   identity
   :component todo/todo-app})

(defuitest todo-test--clear-completed
  [scenes {:system      todo/system
           :system/args []
           :shutdown-f  identity
           :ctx-f       identity
           :props-f     identity
           :component   todo/todo-app}]

  (with-component-mounted [initial-render (rehook.test/mount! scenes)]
    (is initial-render "Initial render should show 4 TODO items"
      (= (rehook.test/children :clear-completed) ["Clear completed " 4]))

    (io initial-render "Click 'Clear completed'"
      (rehook.test/invoke-prop :clear-completed :onClick [{}]))

    (with-component-mounted [render2 (rehook.test/mount! scenes initial-render)]
      (is render2 "After clicking 'Clear Completed' it should clear TODO items"
        (nil? (rehook.test/children :clear-completed)))

      (io render2 "Invoking todo-input onChange"
        (rehook.test/invoke-prop :todo-input :onChange [(clj->js {:target {:value "foo"}})]))

      (with-component-mounted [render3 (rehook.test/mount! scenes render2)]
        (is render3 "After inputting text, value should appear in input"
          (= "foo" (rehook.test/get-prop :todo-input :value)))

        (io render3 "Pressing enter button on todo-input"
          (rehook.test/invoke-prop :todo-input :onKeyDown [(clj->js {:which 13})]))

        (with-component-mounted [render4 (rehook.test/mount! scenes render3)]
          (is render4 "After pressing enter button there should be one item to do"
            (= 1 1))

          (is render4 "After pressing enter button, input's value should be cleared."
            (empty? (rehook.test/get-prop :todo-input :value))))))))

(defn todo-test []
  ((:test (meta #'todo-test--clear-completed))))

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

#_(defn ^:dev/after-load render! []
  (let [test-result (todo-test)]
    (js/console.log "rendering rehook.test ~~~ ♪┏(・o･)┛♪")
    (react-dom/render
     (dom.browser/bootstrap test-result identity clj->js rehook-test-container)
     (js/document.getElementById "app"))))

#_(defn main []
  (render!))