(ns rehook.test.browser
  (:require [rehook.test :as rehook.test :refer-macros [with-component-mounted]]
            [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui]]
            [cljs.test :refer-macros [deftest is testing]]
            [rehook.dom.browser :as dom.browser]
            ["react-dom" :as react-dom]
            ["react-highlight" :as Highlight]
            ["react-frame-component" :as Frame]
            [zprint.core :as zp]
            [clojure.string :as str]
            [sablono.core :as html :refer-macros [html]]
            [clojure.walk :as walk]
            [clojure.data :as data]
            [rehook.demo.todo :as todo]))

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
  (let [scenes (rehook.test/timeline todo/system identity clj->js todo/todo-app)]
    (with-component-mounted [scene1 (rehook.test/mount! scenes)]

      (rehook.test/invoke-prop scene1 :clear-completed :onClick {})

      (with-component-mounted [_ (rehook.test/mount! scenes scene1)]
        @scenes))))

;;(simple-ui-test)

(defui tabs
  [_ props $]
  ($ :div {:style {:display "flex"
                   :justifyContent "start"
                   :flexWrap "wrap"}}
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
  [_ props $]
  (let [{:keys [scene prev-scene index]} (js->clj props :keywordize-keys true)]
    ($ (aget Highlight "default")
       {:language "clojure"
        :key      (scene-key index "code")}
       (with-out-str
        (zp/zprint (js->clj ((:dom scene))) 80)))))

(defui diff
  [_ props $]
  (let [{:keys [scene prev-scene index]} (js->clj props :keywordize-keys true)]
    ($ (aget Highlight "default")
       {:language "clojure"
        :key      (scene-key index "code-diff")}
       (with-out-str
        (zp/zprint

         (data/diff ((:dom scene))
                    ((:dom prev-scene)))
         80)))))

(defui demo-dsl
  [_ props $]
  ($ (aget Highlight "default")
     {:language "clojure"}
     (with-out-str
      (zp/zprint
       [[:assert ["Initial rendered value should be equal foo"
                  (list '= "foo" (list 'first (list 'rehook.test/children '% :my-div)))]]

        [:io! (list 'rehook.test/invoke-prop '% :my-div :onClick {})]

        [:assert ["Rendered value after clicking on div should equal bar"
                  (list '= "bar" (list 'first (list 'rehook.test/children '% :my-div)))]]]
       80
       )

      )))

(defui dom
  [_ props $]
  (let [{:keys [scene]} (js->clj props :keywordize-keys true)]
    ($ (aget Frame "default") {}
       (html (js->clj ((:dom scene)))))))

(defui state
  [_ props $]
  (let [{:keys [scene prev-scene]} (js->clj props :keywordize-keys true)
        state      (some-> scene :state deref)
        prev-state (some-> prev-scene :state deref)]
    (if state
      ($ :div {:style {:overflowX "auto"}}
         ($ :table {}
            ($ :thead {}
               ($ :tr {}
                  ($ :th {} "key")
                  ($ :th {} "index")
                  ($ :th {} "initial value")
                  ($ :th {} "previous value")
                  ($ :th {} "current value")))
            (apply $ :tbody {}
                   (map (fn [[[k i :as id] {:keys [current-value initial-value]}]]
                          ($ :tr {}
                             ($ :td {} (pr-str k))
                             ($ :td {} i)
                             ($ :td {}
                                ($ (aget Highlight "default")
                                   {:language "clojure"}
                                   (with-out-str
                                    (zp/zprint (if (= current-value initial-value)
                                                 "..."
                                                 initial-value)
                                               120))))
                             ($ :td {}
                                ($ (aget Highlight "default")
                                   {:language "clojure"}
                                   (with-out-str
                                    (zp/zprint
                                     (if-let [previous-value (get-in prev-state [id :current-value])]
                                       (if (= current-value previous-value)
                                         "..."
                                         previous-value))
                                     120))))
                             ($ :td {}
                                ($ (aget Highlight "default")
                                   {:language "clojure"}
                                   (with-out-str
                                    (zp/zprint current-value 120))))))
                        state))))
      ($ :div {} "No state mounted"))))

(defui effects
  [_ props $]
  (let [{:keys [scene prev-scene index]} (js->clj props :keywordize-keys true)
        effects (some-> scene :effects deref)
        prev-effects (some-> prev-scene :effects deref)]

    (if effects
      ($ :div {:style {:overflowX "auto"}}
         ($ :table {}
            ($ :thead {}
               ($ :tr {}
                  ($ :th {} "key")
                  ($ :th {} "index")
                  ($ :th {} "prev deps")
                  ($ :th {} "deps")
                  ($ :th {} "evaled?")))
            (apply $ :tbody {}
                   (map (fn [[[k i :as id] {:keys [deps]}]]
                          (let [prev-deps (get-in prev-effects [id :deps])]
                            ($ :tr {}
                               ($ :td {} (pr-str k))
                               ($ :td {} i)
                               ($ :td {} (pr-str prev-deps))
                               ($ :td {} (pr-str deps))
                               ($ :td {} (pr-str (rehook.test/eval-effect? index prev-deps deps))))))
                        effects))))
      ($ :div {} "No effects mounted"))))

(defui tags
  [_ props $]
  (let [{:keys [scene prev-scene]} (js->clj props :keywordize-keys true)
        elements      (some-> scene :elements deref not-empty)
        prev-elements (some-> scene :elements deref)]
    (if  elements
      ($ :div {:style {:overflowX "auto"}}
         ($ :table {}
            ($ :thead {}
               ($ :tr {}
                  ($ :th {} "key")
                  ($ :th {} "props")
                  ($ :th {} "evaled")
                  ($ :th {} "children")))

            (apply $ :tbody {}
                   (map (fn [[k {:keys [args evaled children]}]]
                          ($ :tr {}
                             ($ :td {} (pr-str k))
                             ($ :td {}
                                ($ (aget Highlight "default")
                                   {:language "clojure"}
                                   (with-out-str
                                    (zp/zprint args 80))))
                             ($ :td {} (pr-str evaled))
                             ($ :td {} (pr-str children))))
                        elements))))
      ($ :div {} "No tags found. Stick :rehook/id into one of your props :)"))))

(defui elements
  [_ props $]
  (let [{:keys [scene index]} (js->clj props :keywordize-keys true)
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
  [_ props $]
  (let [[show-summary? set-show-summary] (rehook/use-state true)
        [show-tags? set-show-tags] (rehook/use-state false)
        [show-hiccup? set-show-hiccup] (rehook/use-state false)
        [show-state? set-show-state] (rehook/use-state false)
        [show-effects? set-show-effects] (rehook/use-state false)
        [show-dom? set-show-dom] (rehook/use-state false)
        [show-diff? set-show-diff] (rehook/use-state false)

        {:keys [scene prev-scene index]} (js->clj props :keywordize-keys true)]
    ($ :div {}
       ($ toggle-heading {:title "summary"
                          :onClick set-show-summary
                          :value show-summary?})

       (when show-summary?
         ($ :p {} "3/3 assertions passed"))

       ($ toggle-heading {:title "tags"
                          :onClick set-show-tags
                          :value show-tags?})
       (when show-tags?
         ($ tags {:scene scene
                  :prev-scene prev-scene}))

       ($ toggle-heading {:title "state"
                          :onClick set-show-state
                          :value show-state?})
       (when show-state?
         ($ state {:scene scene
                   :prev-scene prev-scene}))

       ($ toggle-heading {:title "effects"
                          :onClick set-show-effects
                          :value show-effects?})
       (when show-effects?
         ($ effects {:scene scene
                     :prev-scene prev-scene
                     :index index}))


       ($ toggle-heading {:title   "hiccup"
                          :onClick set-show-hiccup
                          :value   show-hiccup?})
       (when show-hiccup?
         ($ code {:scene scene}))

       ($ toggle-heading {:title   "dom"
                          :onClick set-show-dom
                          :value   show-dom?})
       (when show-dom?
         ($ dom {:index index
                 :scene scene}))

       (when prev-scene
         ($ toggle-heading {:title   "diff"
                            :onClick set-show-diff
                            :value   show-diff?}))

       (when (and prev-scene show-diff?)
         ($ diff {:index index
                  :prev-scene prev-scene
                  :scene scene})))))

(def scenes
  (todo-test))

(defui testcard
  [_ _ $]
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
                  ($ :span {:style {:textDecoration "underline"
                                    :color "white"}}
                     (str "Scene " %1))
                  ($ :span {:style   {:color "white"}
                            :onClick (fn [_] (set-index %1))}
                     (str "Scene " %1))))
            timeline)))

       ($ render-scene
          {:key   (str "scene-" current-index)
           :index current-index
           :prev-scene (when (pos? current-index)
                         (get timeline (dec current-index)))
           :scene (get timeline current-index)}))))

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
        ($ :li {} "both react-dom and react-native")
        ($ :li {} "cljs.test + nodejs target for headless/CI")
        ($ :li {} "browser for devcards-like interactive development"))

     ($ :h2 {} "Time-travel driven development")

     ($ :p {} "Writing tests for rehook is not dissimilar to how you might test with datomic or kafka's TopologyTestDriver.")
     ($ :p {} "Each state change produces a snapshot in time that rehook captures as a 'scene'.")

     ($ :p {} "Like kafka's ToplogyTestDriver, the tests run in a simulated library runtime.")
     ($ :p {} "However, a read-only snapshot of the dom can be rendered for each scene! This allows you to catch any runtime errors caused by invalid inputs for each re-render.")

     ($ :h2 {} "Usage")
     ($ :p {} "Documentation on usage is for now a WIP. Please check the unit tests written for "
        ($ :a {:href ""} "reax-synth")
        " and "
        ($ :a {:href ""} "todomvc"))

     ($ :h2 {} "Demo")

     ($ :p {} "The demo below is the output of the unit tests written for rehook's own todomvc.")

     ($ testcard)

     ($ :h2 {} "TODOs")
     ($ :ul {}
        ($ :li {}
           "I want Github-level diffs between the previous scene and the next scene's hiccup.")
        ($ :li {} "How can we use clojure spec and perhaps property based testing to put this thing on steroids?"))))

(defn ^:dev/after-load render! []
  (js/console.log "rendering rehook.test ~~~ ♪┏(・o･)┛♪")
  (react-dom/render
   (dom.browser/bootstrap {} identity clj->js rehook-test-container)
   (js/document.getElementById "app")))

(defn main []
  (render!))