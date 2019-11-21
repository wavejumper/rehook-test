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

      (js/console.log "Scene 1 children => " (pr-str (rehook.test/children scene1 :clear-completed)))

      (rehook.test/invoke-prop scene1 :clear-completed :onClick {})

      (with-component-mounted [scene2 (rehook.test/mount! scenes scene1)]


        (js/console.log "Scene 2 children => " (pr-str (rehook.test/children scene2 :clear-completed)))

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
    ($ :div {:style {:overflow "scroll"}}
       ($ highlight
          {:language "clojure"
           :key      (scene-key index "code")}
          (with-out-str
           (zp/zprint (js->clj ((:dom scene))) 80))))))

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
          [:th {} "component"]
          [:th {} "parent"]
          [:th {} "index"]
          [:th {} "previous value"]
          [:th {} "current value"]]]
        (into [:tbody {}]
              (map (fn [[[k i :as id] {:keys [current-value initial-value]}]]
                     [:tr {}
                      [:td {} (last k)]
                      [:td {} (or (-> k butlast last) "-")]
                      [:td {} (dec i)]
                      [:td {}
                       [clojure-highlight {}
                        (zpr-str
                         (get-in prev-state [id :current-value])
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
          [:th {} "component"]
          [:th {} "parent"]
          [:th {} "index"]
          [:th {} "prev deps"]
          [:th {} "deps"]
          [:th {} "evaled?"]]]
        (into [:tbody {}]
              (map (fn [[[k i :as id] {:keys [deps]}]]
                     (let [prev-deps (get-in prev-effects [id :deps])]
                       [:tr {}
                        [:td {} (last k)]
                        [:td {} (or (-> k butlast last) "-")]
                        [:td {} (dec i)]
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
                                 (zpr-str prev-args)]]
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

(defui test-assertion
  [{:keys [scenes]} props]
  (let [index         (aget props "index")
        test          (get-in scenes [:tests index])
        [show-details? set-show-details] (rehook/use-state true)
        [tab set-tab] (rehook/use-state :dom)]

    (rehook/use-effect
     (fn []
       (set-show-details true)
       (constantly nil))
     [(name tab)])

    [:div {:style {}}
     [:div {:style {:display         "flex"
                    :border          "1px solid #ccc"
                    :padding         "10px"
                    :borderRadius    "3px"
                    :color           "#F8F8F8"
                    :justifyContent  "space-between"
                    :alignItems      "center"
                    :flexWrap        "wrap"
                    :marginTop       "20px"
                    :backgroundColor (if (:pass test)
                                       "#77DD77"
                                       "#B74747")}}

      [:div {:style {:width      "50px"
                     :height     "100%"
                     :alignItems "left"}}
       [:i {:className "material-icons"}
        (if (:pass test) "done" "highlight_off")]]

      [:div {:style {:fontWeight "1000"}}
       (:item test)
       [clojure-highlight {} (zpr-str (:sexp test))]]


      [:div {:style {:border          "1px solid #ccc"
                     :padding         "20px"
                     :backgroundColor "#ccc"
                     :fontSize        "24px"}}
       (:scene test)]]

     [:div {:style {:display        "flex"
                    :justifyContent "space-between"
                    :alignItems   "center"
                    :flexWrap     "wrap"}}
      [:div {:style {:display      "flex"
                     :borderRadius "3px"
                     ;:justifyContent "space-between"
                     :alignItems   "center"
                     :flexWrap     "wrap"
                     :marginTop    "10px"
                     :marginBottom "10px"}}

       [:div {:style   {:padding      "10px"
                        :border       (if (= :dom tab)
                                        "1px solid #222"
                                        "1px solid #ccc")
                        :borderRadius "3px"
                        :marginRight  "10px"
                        :cursor       "pointer"}
              :onClick #(set-tab :dom)}
        (if (= :dom tab)
          [:strong {} "DOM"]
          "DOM")]

       [:div {:style   {:padding       "10px"
                        :border        (if (= :hiccup tab)
                                         "1px solid #222"
                                         "1px solid #ccc")
                        :border-radius "3px"
                        :marginRight   "10px"
                        :cursor        "pointer"}
              :onClick #(set-tab :hiccup)}
        (if (= :hiccup tab)
          [:strong {} "Hiccup"]
          "Hiccup")]

       (when (pos? (:scene test))
         [:div {:style   {:padding      "10px"
                          :border       (if (= :diff tab)
                                          "1px solid #222"
                                          "1px solid #ccc")
                          :cursor       "pointer"
                          :borderRadius "3px"
                          :marginRight  "10px"}
                :onClick #(set-tab :diff)}
          (if (= :diff tab)
            [:strong {} "Diff"]
            "Diff")])

       [:div {:style   {:padding       "10px"
                        :border-radius "3px"
                        :marginRight   "10px"
                        :border        (if (= :effects tab)
                                         "1px solid #222"
                                         "1px solid #ccc")
                        :cursor        "pointer"}
              :onClick #(set-tab :effects)}
        (if (= :effects tab)
          [:strong {} "Effects"]
          "Effects")]

       [:div {:style   {:padding      "10px"
                        :border       (if (= :state tab)
                                        "1px solid #222"
                                        "1px solid #ccc")
                        :cursor       "pointer"
                        :borderRadius "3px"
                        :marginRight  "10px"}
              :onClick #(set-tab :state)}
        (if (= :state tab)
          [:strong {} "State"]
          "State")]]

      [:div {:onClick #(set-show-details (not show-details?))
             :style {:color "blue"
                     :cursor "pointer"}}
       (if show-details? "Hide" "Show")]]

     (when show-details?
       (case tab
         :dom     [dom {:index (:scene test)}]
         :hiccup  [code {:index (:scene test)}]
         :diff    [diff {:index (:scene test)}]
         :effects [effects {:index (:scene test)}]
         :state   [state {:index (:scene test)}]))]))

(defui summary
  [{:keys [scenes]} _]
  (let [tests (:tests scenes)]
    (into [:div {}]
          (map-indexed (fn [idx test]
                 (case (:type test)
                   :assertion
                   [test-assertion {:index idx :key (str "assertions-" idx)}]

                   :mutation
                   [:div {:style {:display         "flex"
                                  :marginTop       "20px"
                                  :border          "1px solid #ccc"
                                  :padding         "10px"
                                  :borderRadius    "3px"
                                  :color           "#F8F8F8"
                                  :justifyContent  "space-between"
                                  :alignItems      "center"
                                  :flexWrap        "wrap"
                                  :backgroundColor "#FCFCFC"}}

                    [:div {:style {:width      "50px"
                                   :height     "100%"
                                   :alignItems "left"}}
                     [:i {:className "material-icons"}
                      "changes"]]

                    [:div {:style {:fontWeight "1000"}}
                     (:item test)
                     [clojure-highlight {} (zpr-str (:mutation test))]]


                    [:div {:style {:border          "1px solid #ccc"
                                   :padding         "20px"
                                   :backgroundColor "#ccc"
                                   :fontSize        "24px"}}
                     (:scene test)
                     [:i {:className "material-icons"}
                      "trending_flat"]
                     (inc (:scene test))]]

                   "mutation"))
               tests))))

(defui render-scene [_ _]
  [:div {} [summary]])

(defui testcard
  [{:keys [scenes]} _ $]
  (let [timeline (:timeline scenes)
        [current-index set-index] (rehook/use-state 0)]
    ($ :div {:style {:border "1px solid #d1d5da"
                     :borderRadius "3px"
                     :padding "15px"}}
       ($ :h2 {} "todomvc: clear completed")

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
  ($ :div {:style {:width       "calc(100% - 128px)"
                   :maxMidth    "680px"
                   :marginLeft  "64px"
                   :marginRight "64px"
                   :fontFamily  "'Open Sans', sans-serif"
                   :lineHeight  "1.5"
                   :color       "#24292e"}}

     ($ heading)

     ($ :h2 {}
        ($ :a {:href "#about"} "About"))

     ($ :p {} "rehook is built from small, modular blocks - each with an explicit notion of time, and a data-first design.")
     ($ :p {} "As rehook is modular, each layer builds upon the last. Each layer adds a new idea: testing, syntax, devtools, patterns.")
     ($ :p {} "rehook's value proposition is that React is the abstraction.")
     ($ :p {} "The core library tries to do two things:")
     ($ :ul {}
        ($ :li {} "marry React hooks with Clojure atoms")
        ($ :li {} "avoid singleton state"))

     ($ :p {} "If you need a primer on what React hooks is, the API docs are a good start.")
     ($ :p {} "React Hook's API shows us that functions are the ultimate interface! The React Hooks API already has many abstractions built on top of it, eg redux style reducers.")
     ($ :p {} "It is my hope that rehook's core API could be used to build general and domain-specific abstractions on top: eg re-frame impls, om-next style querying etc.")

     ($ :h2 {}
        ($ :a {:href "#usage"} "Usage"))

     ($ clojure-highlight {})

     ($ :h2 {}
        ($ :a {:href "#testing"}
           "Testing"))

     ($ :p {} "rehook allows you to test your entire application - from data layer to view.")
     ($ :p {} "rehook-test supports:")
     ($ :ul {}
        ($ :li {} "server, react-dom and react-native")
        ($ :li {} "cljs.test + nodejs target for headless/CI")
        ($ :li {} "browser for devcards-like interactive development")
        ($ :li {} "whatever else you can think of. it's just a function call really."))

     ($ :h2 {} "Time-travel driven development")

     ($ :p {} "Writing tests for rehook is not dissimilar to how you might test with datomic or kafka's TopologyTestDriver, with a bit of devcards in the mix.")
     ($ :p {} "Each state change produces a snapshot in time that rehook captures as a 'scene'.")

     ($ :p {} "Like kafka's ToplogyTestDriver, the tests run in a simulated library runtime.")
     ($ :p {} "However, a read-only snapshot of the dom is rendered for each scene! This allows you to catch any runtime errors caused by invalid inputs for each re-render. ")

     ($ :h2 {}
        ($ :a {:href "#todos"} "TODOs (help wanted)"))
     ($ :ul {}
        ($ :li {} "Polish/package rehook-test for mass-consumption")
        ($ :li {} "I want Github-level diffs between the previous scene and the next scene's hiccup. "
           ($ :a {:href "https://github.com/praneshr/react-diff-viewer" :target "_blank"} "react-diff-viewer?"))
        ($ :li {} "How can we use clojure spec and perhaps property based testing to put this thing on steroids? Eg, instrument and render the shrunk result")
        ($ :li {} "This tool could be used during regular live-reload development. Eg, reframe10x but on even more steroids :)")
        ($ :li {} "As it stands, it's 100% possible to render async tests (eg, test effects that use js/setTimeout, core.async, etc) -- document, explain etc.")
        ($ :li {} "This tool **could** lint/detect various warnings/runtime problems. Eg, when a :key on a component is required, when state/effects are setup incorrectly, etc. Even better, the simulated runtime could detect/profile poor performing code :)"))

     ($ :h2 {}
        ($ :a {:href "#demo"} "Demo"))
     ($ :p {} "The demo below is the output of the unit tests written for rehook's own todomvc.")
     ($ :p {} "You can view the source code here.")
     ($ :p {} "You can see the tests running in a headless CI environment here.")

     ($ testcard)))

(defn ^:dev/after-load render! []
  (let [scenes (todo-test)]
    (js/console.log "rendering rehook.test ~~~ ♪┏(・o･)┛♪")
    (react-dom/render
     (dom.browser/bootstrap {:scenes scenes} identity clj->js rehook-test-container)
     (js/document.getElementById "app"))))

(defn main []
  (render!))