(ns rehook-test
  (:require [rehook.test :as rehook.test]
            [rehook.core :as rehook]
            [rehook.dom :refer-macros [defui]]
            [cljs.test :refer-macros [deftest is testing]]))

;;;; simple-ui

(defui simple-ui
  [_ _ $]
  (let [[x set-x] (rehook/use-state "foo")]
    ($ :div {:onClick   #(set-x "bar")
             :rehook/id :my-div}
       x)))

(deftest simple-ui-test
  (let [scenes (rehook.test/component->scenes {} identity clj->js simple-ui)
        scene1 (rehook.test/play-scenes! scenes 0)]

    (testing "Rendered value should be equal foo"
      (is (= "foo" (first (rehook.test/children scene1 :my-div)))))

    (rehook.test/invoke-prop scene1 :my-div :onClick {})

    (testing "Rendered value after clicking on div should equal bar"
      (let [scene2 (rehook.test/play-scenes! scenes 0)]
        (is (= "bar" (first (rehook.test/children scene2 :my-div))))))))

;;;; simple-ui-with-atom

(defui simple-ui-atom
  [{:keys [my-atom]} _ $]
  (let [[x _] (rehook/use-atom my-atom)]
    ($ :div {:rehook/id :my-div}
       x)))

(deftest simple-ui-atom-test
  (let [my-atom (atom "foo")
        scenes  (rehook.test/component->scenes {:my-atom my-atom} identity clj->js simple-ui-atom)
        scene1  (rehook.test/play-scenes! scenes 0)]

    (testing "Rendered value should equal foo"
      (is (= "foo" (first (rehook.test/children scene1 :my-div)))))

    (reset! my-atom "bar")

    (let [scene2 (rehook.test/play-scenes! scenes 0)]
      (is (= "bar" (first (rehook.test/children scene2 :my-div)))))))