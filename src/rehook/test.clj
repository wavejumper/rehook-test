(ns rehook.test
  (:require [clojure.spec.alpha :as s]
            [rehook.util :as util]))

(s/def ::init-args
  (s/cat :ctx     (s/or :map map?
                        :symbol symbol?)
         :ctx-f   (s/or :fn fn?
                        :symbol? symbol?)
         :props-f (s/or :fn? fn?
                        :symbol? symbol?)
         :e       util/rehook-component?))

(s/def ::defuitest
  (s/cat :name symbol?
         :args (s/tuple symbol? ::init-args)
         :body (s/* any?)))

(defmacro io [scene msg form]
  `(binding [*scene* ~scene]
     (when (nil? *report*)
       (throw (ex-info "io called outside of test" {:scene *scene* :form '~form})))

     (try ~form
          (swap! *report* conj {:scene (:index *scene*)
                                :form  '~form
                                :type  :mutation
                                :msg   ~msg})
          (catch Throwable e#
            (throw (ex-info "io failed" {:scene  *scene*
                                         :form   '~form
                                         :report (deref *report*)}
                            e#))))))

(defmacro is [scene msg form]
  `(binding [*scene* ~scene]
     (when (nil? *report*)
       (throw (ex-info "assertion called outside of test" {:scene *scene* :form '~form})))

     (let [res# ~form]
       (cljs.test/testing ~msg
         (cljs.test/is res#))

       (swap! *report* conj {:scene (:index *scene*)
                             :form  '~form
                             :type  :assertion
                             :msg   ~msg
                             :pass  (if res# true false)}))))

(defmacro with-component-mounted
  [[component-sym component] & body]
  `(let [component# ~component]
     (try (let [~component-sym component#]
            ~@body)
          (finally
            (rehook.test/unmount! component#)))))

(defmacro defuitest [name [scenes [ctx ctx-f props-f e]] & body]
  `(binding [*report* (atom [])]
     (let [~scenes (rehook.test/init ~ctx ~ctx-f ~props-f ~e)]
       ~@body
       (deref *report*))))

#_(is {:xyz ""} "XYZ shoud do blah"
      (inc ))