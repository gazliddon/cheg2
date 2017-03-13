(ns servalan.main
  (:require 
    [clojure.core.async :refer [<!! >!! <! >! put! close! go ] :as a]  
    [clj-uuid :as uuid]
    [chord.http-kit :refer [with-channel wrap-websocket-handler]]
    [org.httpkit.server :refer [run-server]]
    [clojure.pprint :as pp])
  (:gen-class))

(defn to-secs [t]
  (double (/ t 1000000000)))

(defn timer [millis]
  (let [c (a/chan (a/dropping-buffer 1))]
    (a/go
      (loop [start (System/nanoTime)
             t start ]
        (<! (a/timeout millis))
        (let [nt (System/nanoTime)
              dt (- nt t) ]
          (put! c {:time (to-secs (- nt start) ) :dtime (to-secs dt )})
          (recur start (System/nanoTime)))))
    c))

(def all-players (atom []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def objs [{:id 0
            :type :frutz
            :start 0
            :duration 1.5
            :pos [10 10]
            :col [1 1 0] }

           {:id 1
            :type :frutz
            :start 0.5
            :duration 3
            :pos [30 10]
            :col [1 0 1] } ])

(defn in-range? [t s d]
  (and (>= t s) (< t (+ s d))))

(defn obj-in-range? [t {:keys [start duration]}]
  (in-range? t start duration))

(defn active-objs [t objs]
  (->
    (fn [os o]
      (if (obj-in-range? t o)
        (conj os o)
        os))
    (reduce '() objs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server


(defonce server (atom nil))

(def players (atom {}))

(defn keyword-uuid []
  (keyword (str (uuid/v4)) ))

(defn mk-player [{:keys [ws-channel] :as req}]
  {:type :player
   :remote (:remote-addr req)
   :id (keyword-uuid)
   :ws-channel ws-channel })



(defn msg-player! [player msg event-time]
  (a/put! (:to-player player){:msg  msg  :event-time event-time}))

(defn broadcast! [msg event-time]
  (dorun
    (->
      (fn [p]
        (msg-player! p {:time event-time} event-time))
      (map @players))))

(defn add-player! [{:keys [id ws-channel ] :as player} event-time]
  (let [to-player (a/chan)
        player (assoc player
                      :to-player to-player) ]
    (go
      (swap! players assoc id player)

      (let [quit? (atom false)]

        (loop []
          (let [[msg p] (a/alts! [ws-channel to-player])]
            (condp = p
              ;; handle messages coming from the client
              ws-channel (if (nil? msg)
                           (msg-player! player :quit 0)
                           (comment "handle the msg here"))

              ;; handle messages going to the client
              to-player (if (= ( :msg msg ) :quit)
                          (do
                            ;; quit!
                            (reset! quit? true)
                            (swap! players dissoc id ))
                          (>! ws-channel msg))))
          (when-not @quit? (recur)))))

    player))

(def to-server (a/chan))

(defn connection [{:keys [ws-channel] :as req}]
  (let [player (add-player! (mk-player req))]
      (msg-player! player :joined 0)))


(defn stop-server! []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 300ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 300)
    (reset! server nil)))

(defn start-server! [handler]
  (stop-server!)
  (reset! server (run-server (-> handler wrap-websocket-handler) {:port 6502})))

(comment 
  (start-server! connection)
  (stop-server!))

(defn -main [& args]
  (println "here I am!")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




