(ns rehook.test)

(defmacro with-component-mounted
  [[component-sym component] & body]
  `(let [component# ~component]
     (try
       (let [~component-sym component#]
         ~@body)
       (finally
         (rehook.test/unmount! component#)))))

(defmacro defuitest [ ]

  )