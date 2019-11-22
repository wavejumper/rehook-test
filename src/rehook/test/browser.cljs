(ns rehook.test.browser
  (:require [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui ui]]
            [rehook.dom.browser :as dom.browser]
            ["react-highlight" :as Highlight]
            ["react-frame-component" :as Frame]
            ["react-error-boundary" :as ErrorBoundary]
            [zprint.core :as zp]
            [clojure.string :as str]
            [clojure.data :as data]))

(goog-define HTML
  "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"styles/todo.css\"></head><body><div></div></body></html>")

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

(defui code [{:keys [scenes]} props $]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene (current-scene scenes index)]
    ($ error-boundary
       {:FallbackComponent (error-handler {:title "Error rendering Hiccup. You likely found a bug with rehook."} $)}
       ($ :div {:style {:overflow "scroll"}}
          ($ highlight
             {:language "clojure"
              :key      (scene-key index "code")}
             (with-out-str
              (zp/zprint (js->clj ((:dom scene))) 80)))))))

(defui diff [{:keys [scenes]} props $]
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

(defui dom [{:keys [scenes]} props $]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene (current-scene scenes index)
        dom   (:dom scene)]
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

(defui state [{:keys [scenes]} props]
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

(defui effects [{:keys [scenes]} props]
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

(defui tags [{:keys [scenes]} props]
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

(defui elements [{:keys [scenes]} props $]
  (let [{:keys [index]} (js->clj props :keywordize-keys true)
        scene (current-scene scenes index)
        elements (some-> scene :elements deref)]
    (mapv (fn [[k args]]
            ($ :div {:key (scene-key index "elements" (name k))}
               (pr-str args)))
          elements)))

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

(defui test-assertion [{:keys [tests]} props]
  (let [index         (aget props "index")
        test          (get tests index)
        [show-details? set-show-details] (rehook/use-state true)
        [tab set-tab] (rehook/use-state :dom)]

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
         :dom     [dom {:index (:scene test)}]
         :hiccup  [code {:index (:scene test)}]
         :diff    [diff {:index (:scene test)}]
         :effects [effects {:index (:scene test)}]
         :state   [state {:index (:scene test)}]))]))

(defui mutation [{:keys [tests]} props]
  (let [index (aget props "index")
        test  (get tests index)]
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
                    :width           "70px"}}
      (:scene test)
      [material-icon {:icon "trending_flat"}]
      (inc (:scene test))]]))

(defui summary [{:keys [tests]} _]
  (into [:div {}]
        (map-indexed
         (fn [idx test]
           (case (:type test)
             :assertion [test-assertion {:index idx :key (str "assertions-" idx)}]
             :mutation [mutation {:index idx :key (str "mutation-" idx)}]))
         tests)))

(defui testcard [{:keys [name form]} _]
  (let [test-str (zpr-str (first form))]
    [:div {:style {:border       "1px solid #d1d5da"
                   :borderRadius "3px"
                   :padding      "15px"}}
     [:h2 {} name]
     [clojure-highlight {} test-str]
     [summary]]))

(defn render-test [test]
  (dom.browser/bootstrap test identity clj->js testcard))