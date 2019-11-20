(ns rehook.test.browser
  (:require [rehook.test :as rehook.test :refer-macros [with-component-mounted]]
            [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui ui]]
            [cljs.test :refer-macros [deftest is testing]]
            [rehook.dom.browser :as dom.browser]
            ["react-dom" :as react-dom]
            ["react-highlight" :as Highlight]
            ["react-frame-component" :as Frame]
            [zprint.core :as zp]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.data :as data]
            [rehook.demo.todo :as todo]))

(goog-define HTML
  "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"styles/todo.css\"></head><body><div></div></body></html>")

(def highlight
  (aget Highlight "default"))

(defn zpr-str
  ([code]
   (zpr-str code 80))
  ([code numeric-width]
   (with-out-str
    (zp/zprint code (or numeric-width 80)))))

(defui clojure-highlight [_ props $]
  (apply $ highlight {:language "clojure"} (aget props "children")))

(def frame
  (aget Frame "default"))

(defn current-scene [scenes index]
  (get-in scenes [:timeline index]))

(defn previous-scene [scenes index]
  (let [prev-index (dec index)]
    (when-not (neg? prev-index)
      (current-scene scenes prev-index))))

(defui simple-ui
  [_ _ $]
  (let [[x set-x] (rehook/use-state "foo")]
    ($ :div {:onClick   #(set-x "bar")
             :rehook/id :my-div}
       x)))

(defn simple-ui-test []
  (let [scenes (rehook.test/timeline {} identity clj->js simple-ui)]
    (with-component-mounted [scene1 (rehook.test/mount! scenes)]
      (testing "Rendered value should be equal foo"
        (is (= "foo" (first (rehook.test/children scene1 :my-div)))))

      (rehook.test/invoke-prop scene1 :my-div :onClick {})

      (testing "Rendered value after clicking on div should equal bar"
        (with-component-mounted [scene2 (rehook.test/mount! scenes scene1)]
          (is (= "bar" (first (rehook.test/children scene2 :my-div)))))))
    @scenes))

(defn todo-test []
  (let [scenes (rehook.test/timeline (todo/system) identity clj->js todo/todo-app)]
    (with-component-mounted [scene1 (rehook.test/mount! scenes)]

      (rehook.test/invoke-prop scene1 :clear-completed :onClick {})

      (with-component-mounted [_ (rehook.test/mount! scenes scene1)]
        @scenes))))

;;(simple-ui-test)

(defui tabs
  [_ props $]
  ($ :div {:style {:display        "flex"
                   :justifyContent "start"
                   :flexWrap       "wrap"}}
     (aget props "children")))

(defui tab
  [_ props $]
  ($ :div {:style (merge {:marginRight "10px"
                          :display     "inline"
                          :padding     "5px"
                          :border      "1px solid #ccc"
                          :maxHeight   "40px"
                          :cursor      "pointer"
                          :userSelect  "none"}
                         (js->clj (aget props "style")))}
     (aget props "children")))

(defn scene-key [index & words]
  (str "scene-" index "-" (str/join "-" words)))

(defui code
  [{:keys [scenes]} props $]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene (current-scene scenes index)]
    ($ highlight
       {:language "clojure"
        :key      (scene-key index "code")}
       (with-out-str
        (zp/zprint (js->clj ((:dom scene))) 80)))))

(defui diff
  [{:keys [scenes]} props $]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene (current-scene scenes index)
        prev-scene (previous-scene scenes index)]
    ($ highlight
       {:language "clojure"
        :key      (scene-key index "code-diff")}
       (with-out-str
        (zp/zprint

         (data/diff ((:dom scene))
                    ((:dom prev-scene)))
         80)))))

(defui dom
  [{:keys [scenes]} props $]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene (current-scene scenes index)
        dom   (:dom scene)]
    ($ frame {:initialContent HTML
              :style {:height "400px"
                      :width "100%"}}
       ;; bootstrap iframe with 'sandboxed' ctx
       (dom.browser/bootstrap
        {} identity identity
        (ui [_ _]
          (dom))))))

(defui state
  [{:keys [scenes]} props]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene      (current-scene scenes index)
        prev-scene (previous-scene scenes index)
        state      (some-> scene :state deref)
        prev-state (some-> prev-scene :state deref)]
    (if state
      [:div {:style {:overflowX "auto"}}
       [:table {}
        [:thead {}
         [:tr {}
          [:th {} "key"]
          [:th {} "index"]
          [:th {} "initial value"]
          [:th {} "previous value"]
          [:th {} "current value"]]]
        (into [:tbody {}]
              (map (fn [[[k i :as id] {:keys [current-value initial-value]}]]
                     [:tr {}
                      [:td {} (pr-str k)]
                      [:td {} i]
                      [:td {}
                       [clojure-highlight {}
                        (zpr-str
                         (if (= current-value initial-value)
                           "..."
                           initial-value)
                         120)]]
                      [:td {}
                       [clojure-highlight {}
                        (zpr-str
                         (if-let [previous-value (get-in prev-state [id :current-value])]
                           (if (= current-value previous-value)
                             "..."
                             previous-value))
                         120)]]
                      [:td {}
                       [clojure-highlight {} (zpr-str current-value 120)]]])
                   state))]]

      [:div {} "No state mounted"])))

(defui effects
  [{:keys [scenes]} props]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene        (current-scene scenes index)
        prev-scene   (previous-scene scenes index)
        effects      (some-> scene :effects deref)
        prev-effects (some-> prev-scene :effects deref)]
    (if effects
      [:div {:style {:overflowX "auto"}}
       [:table {}
        [:thead {}
         [:tr {}
          [:th {} "key"]
          [:th {} "index"]
          [:th {} "prev deps"]
          [:th {} "deps"]
          [:th {} "evaled?"]]]
        (into [:tbody {}]
              (map (fn [[[k i :as id] {:keys [deps]}]]
                     (let [prev-deps (get-in prev-effects [id :deps])]
                       [:tr {}
                        [:td {} (pr-str k)]
                        [:td {} i]
                        [:td {} (pr-str prev-deps)]
                        [:td {} (pr-str deps)]
                        [:td {} (pr-str (rehook.test/eval-effect? index prev-deps deps))]]))
                   effects))]]

      [:div {} "No effects mounted"])))

(defui tags
  [{:keys [scenes]} props]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene         (current-scene scenes index)
        prev-scene    (previous-scene scenes index)
        elements      (some-> scene :elements deref not-empty)
        prev-elements (some-> prev-scene :elements deref)]
    (if elements
      [:div {:style {:overflowX "auto"}}
       [:table {}
        [:thead {}
         [:tr {}
          [:th {} "key"]
          [:th {} "prev props"]
          [:th {} "props"]
          [:th {} "evaled"]
          [:th {} "children"]]]
        (into [:tbody {}]
              (map (fn [[k {:keys [args evaled children]}]]
                     (let [prev-args (get-in prev-elements [k :args])]
                       [:tr {:key (pr-str index "-" k)}
                        [:td {} (pr-str k)]
                        [:td {} [clojure-highlight {}
                                 (zpr-str (if (= prev-args args)
                                            "..."
                                            prev-args))]]
                        [:td {} [clojure-highlight {} (zpr-str args)]]
                        [:td {} (pr-str evaled)]
                        [:td {} (pr-str children)]])))
              elements)]]

      [:div {} "No tags found. Stick :rehook/id into one of your props :)"])))

(defui elements
  [{:keys [scenes]} props $]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene (current-scene scenes index)
        elements (some-> scene :elements deref)]
    (mapv (fn [[k args]]
            ($ :div {:key (scene-key index "elements" (name k))}
               (pr-str args)))
          elements)))

(defui toggle-heading
  [_ props $]
  (let [title    (aget props "title")
        on-click (aget props "onClick")
        value    (aget props "value")]
    ($ :div {:style {:display        "flex"
                     :justifyContent "space-between"
                     :alignItems     "center"
                     :flexWrap       "wrap"
                     :borderBottom   "1px solid #ccc"
                     :maxHeight      "60px"}}
       ($ :h2 {:style {:marginRight "20px"}}
          title)

       ($ :span {:onClick #(on-click (not value))
                 :style   {:cursor "pointer"
                           :userSelect "none"
                           :color  "blue"}}
          (if value "hide" "show")))))

(defui render-scene
  [{:keys [scenes]} props]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        prev-scene (previous-scene scenes index)
        [show-summary? set-show-summary] (rehook/use-state true)
        [show-tags? set-show-tags]       (rehook/use-state true)
        [show-hiccup? set-show-hiccup]   (rehook/use-state false)
        [show-state? set-show-state]     (rehook/use-state true)
        [show-effects? set-show-effects] (rehook/use-state true)
        [show-dom? set-show-dom]         (rehook/use-state true)
        [show-diff? set-show-diff]       (rehook/use-state false)]

    [:div {}
     [toggle-heading {:title   "summary"
                      :onClick set-show-summary
                      :value   show-summary?}]

     (when show-summary?
       [:p {} "3/3 assertions passed"])

     [toggle-heading {:title   "dom"
                      :onClick set-show-dom
                      :value   show-dom?}]
     (when show-dom?
       [dom {:index index}])

     [toggle-heading {:title   "tags"
                      :onClick set-show-tags
                      :value   show-tags?}]

     (when show-tags?
       [tags {:index index}])

     [toggle-heading {:title   "state"
                      :onClick set-show-state
                      :value   show-state?}]

     (when show-state?
       [state {:index index}])

     [toggle-heading {:title   "effects"
                      :onClick set-show-effects
                      :value   show-effects?}]

     (when show-effects?
       [effects {:index index}])

     [toggle-heading {:title   "hiccup"
                      :onClick set-show-hiccup
                      :value   show-hiccup?}]
     (when show-hiccup?
       [code {:index index}])

     (when prev-scene
       [toggle-heading {:title   "diff"
                        :onClick set-show-diff
                        :value   show-diff?}])

     (when (and prev-scene show-diff?)
       [diff {:index index}])]))

(defui testcard
  [{:keys [scenes]} _ $]
  (let [timeline (:timeline scenes)
        [current-index set-index] (rehook/use-state 0)]
    ($ :div {:style {:border "1px solid #d1d5da"
                     :borderRadius "3px"
                     :padding "15px"}}
       ($ :h2 {} "Test scenario 1")

       ($ tabs {}
          (doall
           (map-indexed
            #($ tab {:key (str "scene-nav-" %1)
                     :style {:backgroundColor "green"}}
                (if (= current-index %1)
                  ($ :strong {:style {:textDecoration "underline"
                                      :color "white"}}
                     (str "Render " %1))
                  ($ :span {:style   {:color "white"}
                            :onClick (fn [_] (set-index %1))}
                     (str "Render " %1))))
            timeline)))

       ($ render-scene
          {:key   (str "scene-" current-index)
           :index current-index}))))

(defui heading
  [_ _ $]
  ($ :h1 {}
     ($ :a {:href "https://github.com/wavejumper/rehook"
            :target "_blank"}
        "rehook")))

(defui rehook-test-container
  [_ _ $]
  ($ :div {:style {:width "calc(100% - 128px)"
                   :maxMidth "680px"
                   :marginLeft "64px"
                   :marginRight "64px"
                   :fontFamily "'Open Sans', sans-serif"
                   :lineHeight "1.5"
                   :color "#24292e"}}

     ($ heading)

     ($ :h2 {} "About")
     ($ :p {} "rehook is my attempt to address the pitfalls of front-end development, and attempt to push it forward.")
     ($ :p {} "rehook is built from small, modular blocks - each with an explicit notion of time, and a data-first design.")
     ($ :p {} "As rehook is modular, each layer builds upon the last. Each layer adds a new idea: testing, syntax, devtools, patterns.")
     ($ :p {} "As rehook is modular, there is not yet a cohesive resource on architecting a rehook app. ")
     ($ :p {} "This is all a work in progress. Please check back :)")


     ($ :h2 {} "Testing")

     ($ :p {} "rehook allows you to test your entire application - from data layer to view.")
     ($ :p {} "rehook-test supports:")
     ($ :ul {}
        ($ :li {} "server, react-dom and react-native")
        ($ :li {} "cljs.test + nodejs target for headless/CI")
        ($ :li {} "browser for devcards-like interactive development")
        ($ :li {} "whatever else you can think of. it's just a function call really."))

     ($ :h2 {} "Time-travel driven development")

     ($ :p {} "Writing tests for rehook is not dissimilar to how you might test with datomic or kafka's TopologyTestDriver.")
     ($ :p {} "Each state change produces a snapshot in time that rehook captures as a 'scene'.")

     ($ :p {} "Like kafka's ToplogyTestDriver, the tests run in a simulated library runtime.")
     ($ :p {} "However, a read-only snapshot of the dom is rendered for each scene! This allows you to catch any runtime errors caused by invalid inputs for each re-render.")

     ($ :h2 {} "Usage")
     ($ :p {} "Documentation on usage is for now a WIP. Please check the unit tests written for "
        ($ :a {:href ""} "reax-synth")
        " and "
        ($ :a {:href ""} "todomvc"))

     ($ :h2 {} "TODOs (help wanted)")
     ($ :ul {}
        ($ :li {} "Polish/package rehook-test for mass-consumption")
        ($ :li {} "I want Github-level diffs between the previous scene and the next scene's hiccup. "
           ($ :a {:href "https://github.com/praneshr/react-diff-viewer" :target "_blank"} "react-diff-viewer?"))
        ($ :li {} "How can we use clojure spec and perhaps property based testing to put this thing on steroids? Eg, instrument and render shrunk result")
        ($ :li {} "This tool could be used during regular live-reload development? Eg, reframe10x but on even more steroids :)")
        ($ :li {} "The keys for state/effects need to be much, much clearer")
        ($ :li {} "This tool **could** lint/detect various warnings/runtime problems. Eg, when a :key on a component is required, when state/effects are setup incorrectly, etc"))

     ($ :h2 {} "Demo")
     ($ :p {} "The demo below is the output of the unit tests written for rehook's own todomvc. You can view the source code here.")

     ($ testcard)))

(defn ^:dev/after-load render! []
  (let [scenes (todo-test)]
    (js/console.log "rendering rehook.test ~~~ ♪┏(・o･)┛♪")
    (react-dom/render
     (dom.browser/bootstrap {:scenes scenes} identity clj->js rehook-test-container)
     (js/document.getElementById "app"))))

(defn main []
  (render!))