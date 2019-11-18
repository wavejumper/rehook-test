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
            [clojure.data :as data]))

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
        (zp/zprint ((:dom scene)) 80)))))

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

(defui dom
  [_ props $]
  (let [{:keys [scene]} (js->clj props :keywordize-keys true)]
    ($ (aget Frame "default") {}
       (html (js->clj ((:dom scene)))))))

(defui effects
  [_ props $]
  ($ :div {} "effects"))

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
                     :justifyContent "start"
                     :alignItems     "center"
                     :flexWrap       "wrap"
                     :border-bottom  "1px solid #ccc"
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
        [show-hiccup? set-show-hiccup] (rehook/use-state true)
        [show-dom? set-show-dom] (rehook/use-state false)
        [show-diff? set-show-diff] (rehook/use-state false)

        {:keys [scene prev-scene index]} (js->clj props :keywordize-keys true)]
    ($ :div {}
       ($ toggle-heading {:title "summary"
                          :onClick set-show-summary
                          :value show-summary?})

       (when show-summary?
         ($ :p {} "3/3 assertions passed"))

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

(defui testcard
  [_ _ $]
  (let [scenes   (simple-ui-test)
        timeline (:timeline scenes)
        [current-index set-index] (rehook/use-state 0)]
    ($ :div {:style {:border  "1px solid #ccc"
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
     ($ :a {:href "https://github.com/wavejumper/rehook-test"}
        "rehook-test")))

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

     ($ testcard)))

(defn ^:dev/after-load render! []
  (js/console.log "rendering rehook.test ~~~ ♪┏(・o･)┛♪")
  (react-dom/render
   (dom.browser/bootstrap {} identity clj->js rehook-test-container)
   (js/document.getElementById "app")))

(defn main []
  (render!))