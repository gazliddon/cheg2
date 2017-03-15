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

(defprotocol IServer
  (start! [this])
  (stop! [this])
  (on-connect! [this req] )
  (on-disconnect! [this client])
  (on-msg! [this client])
  (broadcast-msg! [this client msg]) 
  (send-msg! [this client msg])
  (disconnect-client! [this client]))

(comment defrecord Server []
  (start! [this]
    (->
      this
      stop!
      (fn [this]
        (reset! (:instance this) (run-server (-> (fn [x]) wrap-websocket-handler) {:port 6502})))))

  (stop! [this]
    (let [server (or (:instance this) (atom nil))]
      (when-not (nil? @server)
        (@server :timeout 300)
        (reset! server nil)))
    (assoc :instance server))

  (on-connect! [this req] )
  (on-disconnect! [this client])
  (on-msg! [this client])
  (broadcast-msg! [this client msg]) 
  (send-msg! [this client msg])
  (disconnect-client! [this client]))
 

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


(def objs
  [{:type :frutz
    :start-time 1.5
    :duration 5
    :pos [10 10]
    :col [255 0 0] } 

   {:type :frutz
    :start-time 0
    :duration 5
    :pos [100 30]
    :col [255 0 255]}])

(defonce server (atom nil))

(def players (atom {}))

(defn keyword-uuid []
  (keyword (str (uuid/v4)) ))

(defn mk-player [{:keys [ws-channel] :as req}]
  {:type :player
   :last-ping -1
   :remote (:remote-addr req)
   :id (keyword-uuid)
   :ws-channel ws-channel })

(defn mk-player-msg [typ payload event-time]
  {:type typ :payload payload  :event-time event-time})

(defn msg-player! [player typ payload event-time]
  (a/put! (:to-player player) (mk-player-msg typ payload event-time)))

(def to-server (a/chan))


(defn broadcast! [typ payload event-time]
  (dorun
    (->
      (fn [[k p]]
        (msg-player! p typ payload event-time))
      (map @players))))


(defn valid-join-msg? [msg]
  (and msg
       (= (:type msg) :join )))

(defn get-player [{:keys [id ws-channel ] :as player} timeout]
  player
  )

(defn add-player! [{:keys [id ws-channel ] :as player} event-time]

  (println "add-player!")

  (let [to-player (a/chan)
        player (assoc player
                      :to-player to-player) ]
    (go
      (let [player (get-player player 1000)]
        (println "i have a player")

        (swap! players assoc id player)

        (let [quit? (atom false)]
          ( loop []
            (let [[msg p] (a/alts! [ws-channel to-player])]
              (condp = p
                ;; handle messages coming from the client
                ws-channel (if (nil? msg)
                             (msg-player! player :quit [] 0)
                             (pp/pprint msg))

                ;; handle messages going to the client
                to-player (if (= ( :msg msg ) :quit)
                            (do
                              (>! ws-channel (mk-player-msg :quit [] 0))
                              ;; quit!
                              (reset! quit? true)
                              (swap! players dissoc id ))
                            (>! ws-channel msg))))
            (when-not @quit? (recur))))))))

(defn connection [{:keys [ws-channel] :as req}]
  (do
    (println (str "connection made " ))
    (let [player (add-player! (mk-player req) 0)])))


(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 300)
    (reset! server nil)))

(defn start-server! [handler]
  (stop-server!)
  (reset! server (run-server (-> handler wrap-websocket-handler) {:port 6502})))


(comment 
  (broadcast! :objs [] -1)

  (start-server! #'connection)
  (stop-server!)

  (go
    (let [t-chan (timer 17)]
      (dotimes [n 1000 ]
        (let [t (:time (<! t-chan) )]
          (broadcast! :objs objs t)
          (broadcast! :time [] t)))))
  
  )

(defn -main [& args]
  (println "here I am!"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




