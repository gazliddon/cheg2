(ns client.utils
  (:require
    [dommy.core             :as dommy :include-macros true]   
    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a]))

(defn animate-lo! [callback-fn quit-atom?]
  (when-not @quit-atom?
    (.requestAnimationFrame js/window #(animate-lo! callback-fn quit-atom?))
    (callback-fn)))

(defn animate! [callback-fn]
  "calls callbck-fn at 60hz
   returns func to call to quit"
  (let [quit? (atom false) ]
    (animate-lo! callback-fn quit?)
    (fn []
      (reset! quit? true))))

(defn ch->coll [ch]
  (loop [coll nil]
    (let [v (a/poll! ch)]
      (if-not v
        coll
        (recur (conj coll v))))))

(defn cos [v]
 (.cos js/Math v) )

(defn cos01 [v]
  (/ (+ 1 (cos v) ) 2  ) )

(defn id-ize  [v] (str "#" v))

(defn by-id
  ( [v] (-> (id-ize v) (dommy/sel1)) )
  ( [v b] (->> (id-ize v) (dommy/sel1 b)) ))

(defn get-dims [e] (mapv e [:width :height]))

(defn log-js   [v] (.log js/console v))

(defn to-color [& rgbas]
  (let [csv (apply str (interpose ", " rgbas))]
    (str "rgb(" csv ")")) )

(defn get-win-dims
  "get the dimension of the current window"
  []
  [(.-innerWidth js/window)  
   (.-innerHeight js/window)  ])

