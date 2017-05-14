(ns shared.hostedarray
  )

#?(:clj
   (do
     (defn init-float-array [&v]
       (float-array v)) )

   :cljs
   (do

     (defn init-float-array [&v]
       (let [a (array v)]
         (js/Float32Array a))))
   )

