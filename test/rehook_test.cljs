(ns rehook-test
  (:require [rehook.test :as rehook.test :refer-macros [with-component-mounted]]
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
  (let [scenes (rehook.test/timeline {} identity clj->js simple-ui)]
    (with-component-mounted [scene1 (rehook.test/mount! scenes)]
      (testing "Rendered value should be equal foo"
        (is (= "foo" (first (rehook.test/children scene1 :my-div)))))

      (rehook.test/invoke-prop scene1 :my-div :onClick {})

      (testing "Rendered value after clicking on div should equal bar"
        (with-component-mounted [scene2 (rehook.test/mount! scenes scene1)]
          (is (= "bar" (first (rehook.test/children scene2 :my-div)))))))))

;;;; simple-ui-with-atom

(defui simple-ui-atom
  [{:keys [my-atom]} _ $]
  (let [[x _] (rehook/use-atom my-atom)]
    ($ :div {:rehook/id :my-div}
       x)))

(deftest simple-ui-atom-test
  (let [my-atom (atom "foo")
        scenes  (rehook.test/timeline {:my-atom my-atom} identity clj->js simple-ui-atom)]
    (with-component-mounted [scene1 (rehook.test/mount! scenes)]
      (testing "Rendered value should equal foo"
        (is (= "foo" (first (rehook.test/children scene1 :my-div)))))

      (reset! my-atom "bar")

      (with-component-mounted [scene2 (rehook.test/mount! scenes scene1)]
        (is (= "bar" (first (rehook.test/children scene2 :my-div))))))))