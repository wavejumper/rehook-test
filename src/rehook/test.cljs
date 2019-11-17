(ns rehook.test
  (:require [rehook.core :as rehook]))

(defn- ctx-transformer [ctx elem]
  (update ctx :reax.test/id conj (pr-str elem)))

(defn- use-state
  [local-state next-scene component-id state-id initial-value]
  (let [curr-state-id (swap! state-id inc)
        current-value (get local-state [component-id curr-state-id] initial-value)]
    [current-value #(when-not (= current-value %)
                      (next-scene (assoc local-state [component-id curr-state-id] %)))]))

(defn- use-effect
  [effects component-id effect-id f deps]
  (let [curr-effect-id (swap! effect-id inc)]
    (swap! effects assoc [component-id curr-effect-id]
           {:deps  deps
            :f     f})))

(defn- handle-type
  [next-elements e ctx $ args raw-args children]
  (let [evaled (if (fn? e)
                 (let [ret (e ctx $)]
                   (if (fn? ret)
                     (ret args)
                     ret))
                 e)]
    (if-let [id (:rehook/id raw-args)]
      (let [elem-meta {:e        e
                       :args     raw-args
                       :evaled   evaled
                       :children children}]
        (swap! next-elements assoc id elem-meta)
        evaled)
      evaled)))

(defn- bootstrap
  ([next-elements next-scene effects local-state ctx ctx-f props-f e]
   (bootstrap next-elements next-scene effects local-state ctx ctx-f props-f e {}))

  ([next-elements next-scene effects local-state ctx ctx-f props-f e args & children]
   (let [ctx          (ctx-transformer (ctx-f ctx e) e)
         component-id (get args :key (:reax.test/id ctx))
         state-id     (atom 0)
         effect-id    (atom 0)]

     (with-redefs [rehook/use-effect (partial use-effect effects component-id effect-id)
                   rehook/use-state  (partial use-state local-state next-scene component-id state-id)]

       (let [$ (partial bootstrap next-elements next-scene effects local-state ctx ctx-f props-f)]
         (into [(handle-type next-elements e ctx $ (props-f args) args children)
                (props-f args)]
               children))))))

(defn unmount-scene! [scene]
  (doseq [[_ umount-f] (:evaled-effects scene)]
    (umount-f)))

(defn- eval-effect? [ticks prev-deps deps]
  (cond
    (= 0 ticks)           true
    (empty? deps)         true
    (not= prev-deps deps) true
    :else                 false))

(defn mount-scene
  [prev-scene scene]
  (let [curr-tick    (:ticks prev-scene)
        curr-effects (some-> scene :effects deref)
        prev-effects (:effects prev-scene)]
    {:render         (:render scene)
     :effects        curr-effects
     :ticks          (inc curr-tick)
     :elements       (some-> scene :elements deref)
     :evaled-effects (->> curr-effects
                          (filter (fn [[id {:keys [deps]}]]
                                    (let [prev-deps (get-in prev-effects [id :deps])]
                                      (eval-effect? curr-tick prev-deps deps))))
                          (map (fn [[id {:keys [f]}]]
                                 [id (f)]))
                          (doall))}))

(defn init-scenes
  [ctx ctx-f props-f e]
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
       (unmount-scene! prev-scene)
       (mount-scene prev-scene scene))
     {:ticks 0}
     (drop index timeline))))

(defn total-scenes [scenes]
  (or (some-> scenes deref :timeline count) 0))

(defn children [scene id]
  (-> scene :elements id :children))

(defn get-prop [scene id k]
  (-> scene :elements id :args k))

(defn invoke-prop [scene id k & args]
  (let [f (get-prop scene id k)]
    (apply f args)))

(defn main []
(js/console.log "rehook.test ~~~ ♪┏(・o･)┛♪"))