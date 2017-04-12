(ns client.utils
  (:require
    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]

    [servalan.fsm :as fsm]

    ; [dommy.core             :as dommy :include-macros true]   
    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a]))

(defn add-fsm [this fsm-key table dispatcher ]
  
  (let [fsm-atom (atom nil) ]

    (when-not (satisfies? fsm/IStateMachine this)
      (t/error (type this) " does not satisfy IStateMachine")
      (assert false))

    (let [new-this (assoc this fsm-key fsm-atom) ]
      (do
        (reset! fsm-atom (fsm/mk-state-machine
                           table
                           (fn [ev payload]
                             (dispatcher new-this ev payload))))
        new-this))))

(defn remove-fsm [this fsm-key]
  (assoc this fsm-key nil))


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

; (defn by-id
;   ( [v] (-> (id-ize v) (dommy/sel1)) )
;   ( [v b] (->> (id-ize v) (dommy/sel1 b)) ))

(defn get-dims [e] (mapv e [:width :height]))

(defn log-js   [v] (.log js/console v))

(defn to-color [& rgbas]
  (let [csv (apply str (interpose ", " (map int rgbas)))]
    (str "rgb(" csv ")")) )

(defn get-win-dims
  "get the dimension of the current window"
  []
  [(.-innerWidth js/window)  
   (.-innerHeight js/window)  ])

