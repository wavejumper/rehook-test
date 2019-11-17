(ns rehook.test)

(defn ctx-transformer [ctx elem]
  (update ctx :reax.test/id conj (pr-str elem)))

(defn use-state
  [local-state next-scene component-id state-id initial-value]
  (let [curr-state-id (swap! state-id inc)
        current-value (get local-state [component-id curr-state-id] initial-value)]
    (js/console.log "State called" (pr-str [component-id curr-state-id]))
    [current-value #(when-not (= current-value %)
                      (next-scene (assoc local-state [component-id curr-state-id] %)))]))

(defn use-effect
  [effects component-id effect-id f deps]
  (let [curr-effect-id (swap! effect-id inc)]
    (js/console.log "Effect called" (pr-str [component-id curr-effect-id]))
    (swap! effects assoc [component-id curr-effect-id]
           {:deps  deps
            :f     f})))

(defn handle-type
  [next-elements e ctx $ args]
  (let [evaled (if (fn? e)
                 (let [ret (e ctx $)]
                   (if (fn? ret)
                     (ret args)
                     ret))
                 e)]
    (if-let [id (get (meta e) :rehook/id)]
      (let [elem-meta {:e e
                       :args args
                       :evaled evaled}]
        (swap! next-elements assoc id elem-meta)
        evaled)
      evaled)))

(defn bootstrap
  ([next-elements next-scene effects local-state ctx ctx-f props-f e]
   (bootstrap next-elements next-scene effects local-state ctx ctx-f props-f e {}))

  ([next-elements next-scene effects local-state ctx ctx-f props-f e args & children]
   (js/console.log "Bootstrap being called"
                   (pr-str {:local-state local-state
                            :ctx ctx
                            :e e
                            :args args}))

   (let [ctx          (ctx-transformer (ctx-f ctx e) e)
         component-id (get args :key (:reax.test/id ctx))
         state-id     (atom 0)
         effect-id    (atom 0)]

     (with-redefs [rehook.core/use-state  (partial use-state local-state next-scene component-id state-id)
                   rehook.core/use-effect (partial use-effect effects component-id effect-id)]

       (into [(handle-type next-elements e ctx (partial bootstrap next-elements next-scene effects local-state ctx ctx-f props-f) (props-f args))]
             children)))))

(defn unmount-scene [scene]
  (doseq [umount-f (:evaled-effects scene)]
    (umount-f))
  scene)

(defn mount-scene
  [prev-scene scene]
  (let [curr-tick    (:ticks prev-scene)
        curr-effects (some-> scene :effects deref)
        prev-effects (:effects prev-scene)]

    (unmount-scene prev-scene)

    {:render         (:render scene)
     :effects        curr-effects
     :ticks          (inc curr-tick)
     :elements       (some-> scene :elements deref)
     :evaled-effects (->> curr-effects
                          (filter (fn [[id {:keys [deps]}]]
                                    (let [prev-deps (get-in prev-effects [id :deps])]
                                      (cond
                                        (= 0 curr-tick) true
                                        (empty? deps) true
                                        (not= prev-deps deps) true
                                        :else false))))
                          (map (fn [{:keys [f]}]
                                 (f))))}))

(defn component->scenes [ctx ctx-f props-f e]
  (let [scenes (atom {:timeline []})]
    (letfn [(next-scene [next-local-state]
              (swap! scenes update :timeline conj
                     (let [next-effects  (atom {})
                           actions       (atom {})
                           next-elements (atom {})]
                       {:actions  actions
                        :render   (bootstrap next-elements next-scene next-effects next-local-state ctx ctx-f props-f e)
                        :effects  next-effects
                        :elements next-elements})))]
      (next-scene {})
      scenes)))

(defn play-scenes!
  [scenes index]
  (let [{:keys [timeline]} @scenes]
    (reduce
     (fn [prev-scene scene]
       (mount-scene prev-scene scene))
     {:ticks 0}
     (drop index timeline))))

(defn main []
  (js/console.log "Hello world"))