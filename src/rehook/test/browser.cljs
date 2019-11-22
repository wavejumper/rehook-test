(ns rehook.test.browser
  (:require [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui ui]]
            [rehook.dom.browser :as dom.browser]
            [rehook.test :as rehook.test]
            [zprint.core :as zp]
            [clojure.string :as str]
            [clojure.data :as data]
            ["react-highlight" :as Highlight]
            ["react-frame-component" :as Frame]
            ["react-error-boundary" :as ErrorBoundary]
            ["react-dom" :as react-dom]))

(goog-define HTML "")
(goog-define target "app")

(def highlight
  (aget Highlight "default"))

(def error-boundary
  (aget ErrorBoundary "default"))

(def frame
  (aget Frame "default"))

(defn zpr-str
  ([code]
   (zpr-str code 80))
  ([code numeric-width]
   (with-out-str
    (zp/zprint code (or numeric-width 80)))))

(defui clojure-highlight [_ props $]
  (apply $ highlight {:language "clojure"} (aget props "children")))

(defn current-scene [scenes index]
  (get-in scenes [:timeline index]))

(defn previous-scene [scenes index]
  (let [prev-index (dec index)]
    (when-not (neg? prev-index)
      (current-scene scenes prev-index))))

(defn scene-key [index & words]
  (str "scene-" index "-" (str/join "-" words)))

(defui error-handler [{:keys [title]} props]
  (let [error      (aget props "error")
        stacktrace (aget props "componentStack")]
    [:div {}
     [:h1 {} (str title)]
     [clojure-highlight {} (zpr-str error 120)]
     [highlight {:language "javascript"} (str stacktrace)]]))

(defui material-icon [_ props]
  (let [icon (aget props "icon")]
    [:i {:className "material-icons"
         :style {:userSelect "none"}}
     icon]))

(defui code [{:keys [test-results]} props $]
  (let [[idx1 idx2] (aget props "path")
        [scenes _]  (rehook/use-atom-path test-results [idx1 :scenes])
        scene       (current-scene scenes idx2)]
    ($ error-boundary
       {:FallbackComponent (error-handler {:title "Error rendering Hiccup. You likely found a bug with rehook."} $)}
       ($ :div {:style {:overflow "scroll"}}
          ($ highlight
             {:language "clojure"
              :key      (scene-key idx2 "code")}
             (with-out-str
              (zp/zprint (js->clj ((:dom scene))) 80)))))))

(defui diff [{:keys [test-results]} props $]
  (let [[idx1 idx2] (aget props "path")
        [scenes _]  (rehook/use-atom-path test-results [idx1 :scenes])
        scene       (current-scene scenes idx2)
        prev-scene  (previous-scene scenes idx2)]
    ($ highlight
       {:language "clojure"
        :key      (scene-key idx2 "code-diff")}
       (with-out-str
        (zp/zprint

         (data/diff ((:dom scene))
                    ((:dom prev-scene)))
         80)))))

(defui dom [{:keys [test-results]} props $]
  (let [[idx1 idx2] (aget props "path")
        [scenes _]  (rehook/use-atom-path test-results [idx1 :scenes])
        scene       (current-scene scenes idx2)
        dom         (:dom scene)]
    ($ error-boundary
       {:FallbackComponent (error-handler {:title "Error rendering to the DOM. You likely found a bug with rehook."} $)}
       ($ frame {:initialContent HTML
                 :style          {:height "400px"
                                  :width  "100%"}}
          ;; bootstrap iframe with 'sandboxed' ctx
          (dom.browser/bootstrap
           {} identity clj->js
           (ui [_ _]
             (dom)))))))

(defui state [{:keys [test-results]} props]
  (let [[idx1 idx2] (aget props "path")
        [scenes _]  (rehook/use-atom-path test-results [idx1 :scenes])
        scene       (current-scene scenes idx2)
        prev-scene  (previous-scene scenes idx2)
        state       (some-> scene :state deref)
        prev-state  (some-> prev-scene :state deref)]
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
              (map (fn [[[k i :as id] {:keys [current-value]}]]
                     [:tr {}
                      [:td {} (last k)]
                      [:td {} (or (-> k butlast last) "-")]
                      [:td {} (dec i)]
                      [:td {}
                       [clojure-highlight {}
                        (zpr-str (get-in prev-state [id :current-value]) 120)]]
                      [:td {}
                       [clojure-highlight {} (zpr-str current-value 120)]]])
                   state))]]

      [:div {} "No state mounted"])))

(defui effects [{:keys [test-results]} props]
  (let [[idx1 idx2]  (aget props "path")
        [scenes _]   (rehook/use-atom-path test-results [idx1 :scenes])
        scene        (current-scene scenes idx2)
        effects      (some-> scene :effects deref)
        prev-scene   (previous-scene scenes idx2)
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
                        [:td {} (pr-str (rehook.test/eval-effect? idx2 prev-deps deps))]]))
                   effects))]]

      [:div {} "No effects mounted"])))

(defui button [_ props]
  (let [on-click (aget props "onClick")
        selected (aget props "selected")
        title    (aget props "title")]
    [:div {:style   {:padding      "10px"
                     :border       (if selected
                                     "1px solid #222"
                                     "1px solid #ccc")
                     :borderRadius "3px"
                     :marginRight  "10px"
                     :cursor       "pointer"
                     :userSelect   "none"}
           :onClick on-click}
     (if selected
       [:strong {} title]
       title)]))

(defui test-assertion [{:keys [test-results]} props]
  (let [[idx1 idx2]                      (aget props "path")
        path                             [idx1 :tests idx2]
        [test _]                         (rehook/use-atom-path test-results path)
        [show-details? set-show-details] (rehook/use-state true)
        [tab set-tab]                    (rehook/use-state :dom)]

    (rehook/use-effect
     (fn []
       (set-show-details true)
       (constantly nil))
     [(name tab)])

    [:div {:style {}}
     [:div {:style {:display         "flex"
                    :border          "1px solid #88CC88"
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
       [material-icon
        {:icon (if (:pass test) "done" "highlight_off")}]]

      [:div {:style {:fontWeight "1000"}}
       (:title test)
       [clojure-highlight {} (zpr-str (:form test) 80)]]

      [:div {:style {:border          "1px solid #ccc"
                     :padding         "20px"
                     :backgroundColor "#ccc"
                     :fontSize        "24px"
                     :textAlign       "center"
                     :userSelect      "none"
                     :width           "70px"}}
       (:scene test)]]

     [:div {:style {:display        "flex"
                    :justifyContent "space-between"
                    :alignItems   "center"
                    :flexWrap     "wrap"}}

      [:div {:style {:display      "flex"
                     :borderRadius "3px"
                     :alignItems   "center"
                     :flexWrap     "wrap"
                     :marginTop    "10px"
                     :marginBottom "10px"}}

       [button {:onClick  #(set-tab :dom)
                :title    "DOM"
                :selected (= :dom tab)}]

       [button {:onClick  #(set-tab :hiccup)
                :title    "Hiccup"
                :selected (= :hiccup tab)}]

       (when (pos? (:scene test))
         [button {:onClick #(set-tab :diff)
                  :selected (= tab :diff)
                  :title    "Diff"}])

       [button {:onClick  #(set-tab :effects)
                :selected (= tab :effects)
                :title    "Effects"}]

       [button {:onClick  #(set-tab :state)
                :selected (= tab :state)
                :title    "State"}]]

      [:div {:onClick #(set-show-details (not show-details?))
             :style {:color "blue"
                     :cursor "pointer"
                     :userSelect "none"}}
       (if show-details? "Hide" "Show")]]

     (when show-details?
       (case tab
         :dom     [dom {:path [idx1 (:scene test)]}]
         :hiccup  [code {:path [idx1 (:scene test)]}]
         :diff    [diff {:path [idx1 (:scene test)]}]
         :effects [effects {:path [idx1 (:scene test)]}]
         :state   [state {:path [idx1 (:scene test)]}]))]))

(defui mutation [{:keys [test-results]} props]
  (let [[idx1 idx2] (aget props "path")
        path        [idx1 :tests idx2]
        [test _]    (rehook/use-atom-path test-results path)]
    [:div {:style {:display         "flex"
                   :marginTop       "20px"
                   :border          "1px solid #ccc"
                   :padding         "10px"
                   :borderRadius    "3px"
                   :color           "#444"
                   :justifyContent  "space-between"
                   :alignItems      "center"
                   :flexWrap        "wrap"
                   :backgroundColor "#FCFCFC"}}

     [:div {:style {:width      "50px"
                    :height     "100%"
                    :alignItems "left"}}
      [material-icon {:icon "changes"}]]

     [:div {:style {:fontWeight "1000"}}
      (:title test)
      [clojure-highlight {} (zpr-str (:form test) 80)]]

     [:div {:style {:border          "1px solid #ccc"
                    :padding         "20px"
                    :backgroundColor "#ccc"
                    :fontSize        "24px"
                    :textAlign       "center"
                    :userSelect      "none"
                    :width           "70px"}}
      (:scene test)
      [material-icon {:icon "trending_flat"}]
      (inc (:scene test))]]))

(defui summary [{:keys [test-results]} props]
  (let [index (aget props "index")
        [{:keys [name ns tests]} _] (rehook/use-atom-path test-results [index])]
    (into [:div {}]
          (map-indexed
           (fn [idx test]
             (case (:type test)
               :assertion [test-assertion {:path [index idx]
                                           :key  (str ns "/" name "/" "assertion-" idx)}]
               :mutation [mutation {:path [index idx]
                                    :key  (str ns "/" name "/" "mutation-" idx)}]))
           tests))))

(defui testcard [{:keys [test-results]} props]
  (let [index (aget props "index")
        [{:keys [name form ns line tests]} _] (rehook/use-atom-path test-results [index])
        test-str (zpr-str (first form))
        title (str ns "/" name ":" line)
        pass? (->> tests
                   (filter #(= :assertion (:type %)))
                   (every? :pass))
        [show-code-snippet? set-show-code-snippet] (rehook/use-state true)
        [expanded? set-expanded] (rehook/use-state (not pass?))]

    [:div {:style {:border       "1px solid"
                   :borderRadius "3px"
                   :borderLeft   "15px solid"
                   :display      "flex"
                   :borderColor  (if pass?
                                   "#77DD77"
                                   "#B74747")
                   :paddingLeft "15px"
                   :paddingRight "15px"
                   :flexDirection "column"}}

     [:div {:style {:cursor "pointer"
                    :flexGrow "1"}
            :onClick #(set-expanded (not expanded?))}

      [:h2 {:style {:color  "#222"}}
       [:span {:style {:marginRight "5px"}}
        [material-icon {:icon (if expanded?
                                "keyboard_arrow_down"
                                "chevron_right")}]]
       title]]

     (when expanded?
       [:div {:onClick #(set-show-code-snippet (not show-code-snippet?))
              :style   {:color      "blue"
                        :cursor     "pointer"
                        :userSelect "none"}}
        (if show-code-snippet? "Hide test form" "Show test form")]


       (when show-code-snippet?
         [clojure-highlight {} test-str])

       [summary {:index index}])]))

(defn run-test!
  [{:keys [test column line end-line end-column ns]}]
  (assoc (test)
    :column     column
    :line       line
    :end-line   end-line
    :end-column end-column
    :ns         ns))

(defn test-stats [test-results]
  (let [tests      (mapcat :tests test-results)
        assertions (filter #(= :assertion (:type %)) tests)]
    {:total-tests      (count test-results)
     :total-assertions (count assertions)
     :pass             (count (filter :pass assertions))
     :fail             (count (filter (comp not :pass) assertions))}))

(defn test-outcome-str
  [{:keys [total-tests total-assertions fail]}]
  (let [test-str      (if (= 1 total-tests) "test" "tests")
        assertion-str (if (= 1 total-assertions) "assertion" "assertions")
        fail-str      (if (= 1 fail) "failure" "failures")]
    (str total-tests " " test-str ", " total-assertions " " assertion-str ", " fail " " fail-str ".")))

(defui report-summary [{:keys [test-results]} _]
  (let [[test-results _] (rehook/use-atom test-results)]

    (let [test-stats (test-stats test-results)
          output     (test-outcome-str test-stats)
          success?   (zero? (:fail test-stats))]

      (into [:div {}
             [:div {:style {:color (if success?
                                     "green"
                                     "red")
                            :marginBottom "30px"}}
              output]]

            (map-indexed
             (fn [i {:keys [name ns]}]
               [testcard {:key (str "test-summary-" ns "/" name)
                          :index i}])
             test-results)))))

(defui rehook-summary [{:keys [registry test-results]} _]
  (let [[registry _] (rehook/use-atom registry)]

    ;; Re-run our tests everytime the registry updates.
    (rehook/use-effect
     (fn []
       (js/console.log "%c running rehook.test report ~~~ ♪┏(・o･)┛♪"
                       "background: #222; color: #bada55")
       (->> registry
            (mapv (fn [[_ var]]
                    (run-test! (meta var))))
            (reset! test-results))
       (constantly nil)))

    [:div {:style {:width       "calc(100% - 128px)"
                   :maxMidth    "680px"
                   :marginLeft  "64px"
                   :marginRight "64px"
                   :fontFamily  "'Open Sans', sans-serif"
                   :lineHeight  "1.5"
                   :color       "#24292e"}}
     [:h1 {}
      [:a {:href "https://github.com/wavejumper/rehook"
           :target "_blank"}
       "rehook-test"]]

     [report-summary]]))

(defn report []
  (react-dom/render
   (dom.browser/bootstrap
    ;; we kinda break our rule of no singleton state here :p
    {:registry rehook.test/registry
     :test-results (atom [])}
    identity
    clj->js
    rehook-summary)
   (js/document.getElementById target)))