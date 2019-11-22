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

(defmacro io [scene title form]
  `(binding [*scene* ~scene]
     (when (nil? *report*)
       (throw (ex-info "io called outside of test" {:scene *scene* :form '~form})))

     (try ~form
          (swap! *report* update :tests conj
                 {:scene (-> *scene* :ticks dec)
                  :form  '~form
                  :type  :mutation
                  :title ~title})
          (catch js/Error e#
            (throw (ex-info "io failed" {:scene  *scene*
                                         :form   '~form
                                         :report (deref *report*)}
                            e#))))))

(defmacro is [scene title form]
  `(binding [*scene* ~scene]
     (when (nil? *report*)
       (throw (ex-info "assertion called outside of test" {:scene *scene* :form '~form})))

     (let [res# ~form]
       (cljs.test/testing ~title
         (cljs.test/is res#))

       (swap! *report* update :tests conj
              {:scene (-> *scene* :ticks dec)
               :form  '~form
               :type  :assertion
               :title ~title
               :pass  (if res# true false)}))))

(defmacro with-component-mounted
  [[component-sym component] & body]
  `(let [component# ~component]
     (try (let [~component-sym component#]
            ~@body)
          (finally
            (rehook.test/unmount! component#)))))

(defmacro defuitest
  [name [scenes args] & body]
  `(defn ~(vary-meta name assoc
                     :rehook/test? true
                     :test `(fn []
                              (binding [*report* (atom {:form  '~body
                                                        :name  ~(str name)
                                                        :tests []})]
                                (let [system#         ~(:system args)
                                      system-args#    ~(:system/args args)
                                      invoked-system# (apply system# system-args#)
                                      ctx-f#          ~(:ctx-f args)
                                      props-f#        ~(:props-f args)
                                      component#      ~(:component args)
                                      shutdown-f#     ~(:shutdown-f args)
                                      scenes#         (rehook.test/init invoked-system# ctx-f# props-f# component#)
                                      ~scenes         scenes#]
                                  (try
                                    ~@body
                                    (assoc (deref *report*) :scenes (deref scenes#))
                                    (finally
                                      (shutdown-f# invoked-system#)))))))
     []
     ::huh))