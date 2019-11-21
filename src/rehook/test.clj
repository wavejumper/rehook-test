(ns rehook.test
  (:require [clojure.spec.alpha :as s]
            [rehook.util :as util]))

(defmacro with-component-mounted
  [[component-sym component] & body]
  `(let [component# ~component]
     (try
       (let [~component-sym component#]
         ~@body)
       (finally
         (rehook.test/unmount! component#)))))

(s/def ::init-args
  (s/cat :ctx (s/or :map map?
                    :symbol symbol?)
         :ctx-f (s/or :fn fn?
                      :symbol? symbol?)
         :props-f (s/or :fn? fn?
                        :symbol? symbol?)
         :e util/rehook-component?))

(s/def ::defuitest
  (s/cat :name symbol?
         :args (s/tuple ::init-args symbol?)
         :body (s/* any?)))