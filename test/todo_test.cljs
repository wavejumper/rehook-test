(ns todo-test
  (:require [rehook.test :as rehook.test :refer-macros [with-component-mounted defuitest is io]]
            [rehook.demo.todo :as todo]))

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


(defuitest todo-stats--items-left
  [scenes {:system      todo/system
           :system/args []
           :shutdown-f  identity
           :ctx-f       identity
           :props-f     identity
           :component   todo/todo-stats}]

  (with-component-mounted [initial-render (rehook.test/mount! scenes)]
    (is initial-render "Initial render should show 0 items left"
      (not= (rehook.test/children :items-left) [0 " items left"]))))