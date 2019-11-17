(ns rehook.test
  (:require [rehook.core :as rehook]
            [rehook.util :as util]))

(defn ctx-transformer [ctx elem]
  (update ctx :reax.test/id conj (util/display-name elem)))

(defn use-state
  [local-state next-scene component-id state-id initial-value]
  (let [curr-state-id (swap! state-id inc)
        current-value (get local-state [component-id curr-state-id] initial-value)]
    [current-value #(next-scene (assoc local-state [component-id curr-state-id] %))]))

(defn use-effect
  [effects ticks component-id effect-id f deps]
  (let [prev-deps (-> effects :prev :deps)]
    (swap! (:curr effects) assoc [component-id effect-id]
           {:deps  deps
            :f     f
            :eval? (cond
                     (= 0 ticks)           true
                     (empty? deps)         true
                     (not= prev-deps deps) true
                     :else                 false)})))

(defn handle-type
  [e ctx $ args]
  (if (util/rehook-component? e)
    ((e ctx $) args)
    e))

(defn bootstrap
  ([ticks next-scene effects local-state ctx ctx-f props-f e]
   (bootstrap ticks next-scene effects local-state ctx ctx-f props-f e {}))

  ([ticks next-scene effects local-state ctx ctx-f props-f e args & children]
   (let [ctx          (ctx-transformer (ctx-f ctx e) e)
         component-id (get args :key (:reax.test/id ctx))
         state-id     (atom 0)
         effect-id    (atom 0)]

     (with-redefs [rehook/use-state  (partial use-state local-state next-scene component-id state-id)
                   rehook/use-effect (partial use-effect effects ticks component-id effect-id)]

       (into [(handle-type e ctx (partial bootstrap next-scene local-state ctx ctx-f props-f) (props-f args))]
             children)))))

(defn mount-scene [scene]
  {:render         (:render scene)
   :effects        (:effects scene)
   :evaled-effects (some->> (:effects scene)
                            (:curr)
                            (deref)
                            (filter :eval?)
                            (map (fn [{:keys [f]}]
                                   (f))))})

(defn unmount-scene [scene]
  (doseq [umount-f (:evaled-effects scene)]
    (umount-f))
  scene)

(defn component->scenes [ctx ctx-f props-f e]
  (let [scenes (atom [])]
    (letfn [(next-scene [next-local-state]
              (swap! scenes
                     (fn [scenes]
                       (let [{:keys [effects]} (last scenes)]
                         (let [next-effects {:prev (some-> effects :curr deref)
                                             :curr (atom {})}]
                           (conj scenes
                                 {:render  (bootstrap (count scenes) next-scene next-effects next-local-state
                                                      ctx ctx-f props-f e)
                                  :effects next-effects}))))))]
      (next-scene {})
      scenes)))

(defn main []
  (js/console.log "Hello world"))