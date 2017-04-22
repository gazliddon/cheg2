(ns client.core
  (:require
    [client.audio :as AU]
    [client.game :as game]
    [cljs.analyzer :as ana]
    [taoensso.timbre :as t
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]

    [sablono.core :as html :refer-macros [html]]

    [com.stuartsierra.component :as c]

    [shared.protocols.clientconnection :as client]
    [client.client :refer [mk-client-component]]

    [shared.messages :refer [mk-msg]]
    [shared.utils :as su]

    [shared.fsm :as fsm]
    [client.protocols :as p]
    [client.html :refer [mk-html-component mk-html-events-component]]
    [client.utils :refer [ch->coll cos cos01] :as u]

    [client.keys :as k]

    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]

    [om.dom :as dom]
    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a])

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(declare start stop restart)


(def config { :conn-config {:url  "ws://localhost:6502"
                            :ping-frequency 1000 }
             :html-id "game" })

(defn mk-bidi-chan []
  (let [reader (chan)
        writer (chan) ]
    (do
      (go-loop
        []
        (when-let [m (<! writer)]
          (>! writer m)))

      (su/bidi-ch reader writer))))

(defn mk-game [config ui-chan ]

  (let [com-rx (chan)
        com-tx (chan)
        com-chan (su/bidi-ch com-tx com-tx) ]

    (c/system-map
      :com-chan com-chan

      :config config

      :system (mk-html-component (:html-id config))

      :events (mk-html-events-component)

      :ui-chan ui-chan

      :client-connection (c/using
                           ( mk-client-component (:conn-config config) )
                           [:com-chan
                            :ui-chan])

      :game (c/using 
              (game/mk-game)
              [:com-chan
               :ui-chan
               :client-connection
               :events
               :system
               :config])
      )  
    )


  )

(defonce sys-atom (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def objs (atom []))

(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force re-rendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(def app-state (atom {:game-status {:running false
                                    :game-state :stopped
                                    :log [] } }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti reader-fn om/dispatch)

(defmethod reader-fn :default
 [{:keys [state]} k _]
  (let [st @state] ;; CACHING!!!
    (if (contains? st k)
      {:value (get st k)})))

(defmulti mutate-fn om/dispatch)

(defmethod mutate-fn `game/set-field
  [{:keys [state] :as env} key {:keys [field value]}]
  {:action (fn[] (swap! state assoc-in [:game-status field] value))})

(def max-log-entries 10)

(defmethod mutate-fn `game/log
  [{:keys [state] :as env} _ {:keys [payload]}]
  (let [log (-> @state :game-status :log )
        new-log (take max-log-entries (conj log payload)) ]
    {:action (fn[] (swap! state assoc-in [:game-status :log] new-log))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mk-button [this txt action]
  [:button
   {:class  "pure-button pure-button-primary"
    :on-click (fn [] (action))}
   txt])

(defn pad-coll [sz padding coll]
  (take sz
        (concat coll (take sz (repeat padding)))))

(defn log-window [this {:keys [log] :as props}]
  (let [lst (->>
              log
              (pad-coll 10 "!")
              (reverse)
              ; (interpose "\n")
              (mapv (fn [v][:p (str v)]))
              (into [:div {:class "log"} ]))]
    (html
      lst)))



(defui App

  static om/IQuery

  (query [this] '[:game-status])

  Object

  (render [this]
          (let [{:keys [game-status] :as props} (om/props this)
                {:keys [running game-state]} game-status ]
            (html
              [:div
               [:h1 "Chuckie Egg"]

               (log-window this game-status)

               [:p (str "Game state: " (-> game-state name str) )]

               (case game-state
                 :stopped (mk-button this "Start" start)
                 :running (mk-button this "Stop" stop)
                 :connecting (mk-button this "Connecting" restart)
                 (mk-button this "" identity)) ]
              ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn decode-audio-data
  [context data]
  (let [ch (chan)]
    (.decodeAudioData context
                      data
                      (fn [buffer]
                        (go (>! ch buffer)
                            (close! ch))))
    ch))

(defonce cache (atom {}))

(defn get-audio [url]
  (let [ch (chan)]
    (doto (goog.net.XhrIo.)
      (.setResponseType "arraybuffer")
      (.addEventListener goog.net.EventType.COMPLETE
                         (fn [event]
                           (let [res (-> event .-target .getResponse)]
                             (go (>! ch res)
                                 (close! ch)))))
      (.send url "GET"))
    ch))


(defonce AudioContext (or (.-AudioContext js/window)
                          (.-webkitAudioContext js/window)))

(defonce context (AudioContext.))

(defonce cache (atom {}))

(defn load-audio-buffer-async
  [url]
  (if-let [dat (get @cache url)]
    (go
      dat)
    (go
      (let [response (<! (get-audio url))
            buffer (<! (decode-audio-data context response)) ]
        (do
          (swap! cache assoc url buffer)
          buffer)))))

(defn make-audio-source [buffer]
  (doto (.createBufferSource context)
    (aset "buffer" buffer)) )

(defn connect
  ([a b]
   (do
     (.connect a b)
     b))
  ([a]
   (connect a (.-destination context))))

(defprotocol ISound
  (play-sound [_])
  (stop-sound [_])
  (set-volume [_ _])
  (fade-out [_ _]))

(defrecord Sound [buffer vol source-atom gain-node]
  ISound

  (play-sound [this]
    (let [ source (make-audio-source buffer) ]
      (do
        (stop-sound this)
        (->
          (connect source gain-node)
          (connect))
        ; (set! (.-loop source) true)
        (set! (.-onended source )
              (fn []
                ))
        (.start source)
        (reset! source-atom source)
        )))

  (stop-sound [this]
    (when @source-atom
      (.stop @source-atom)
      (reset! source-atom nil)))

  (set-volume [this v]
    (set! (-> gain-node .-gain .-value) v))

  (fade-out [this t]

    ))

(defn mk-sound [url]
  (let [gain-node (.createGain context)
        buff-chan (load-audio-buffer-async url) ]
    (go
      (map->Sound {:buffer (<! buff-chan)
                   :source-atom (atom nil)
                   :gain-node gain-node
                   :vol gain-node }))))

(def xx (mk-sound "audio/ping.wav"))

(comment
 (go
  (def yy (<! xx))
  (play-sound yy)
  ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def reconciler

  (om/reconciler
    {:state app-state
     :parser (om/parser {:read reader-fn :mutate mutate-fn})}))

(defn set-ui-field! [k v]
  (om/transact! reconciler `[(game/set-field {:field ~k :value ~v} )]))

(defn set-ui-game-state! [v]
  (set-ui-field! :game-state v))

(defn mk-ui-listener [ch]
  (go-loop []
    (when-let [msg (<! ch)]
      (case (:type msg)
            :game-connecting (set-ui-game-state! :connecting)
            :game-started (set-ui-game-state! :running)
            :game-stopped (set-ui-game-state! :stopped)
            :log (do
                   (om/transact! reconciler `[(game/log ~msg)]))
            :default)
      (recur)
      )
    )
  )

(def ui-chan (chan))

(defn main []

  (let [app-el (gdom/getElement "app") ]
    (do
      (om/add-root! reconciler App app-el)
      ;; listen for any msg to the UI
      (mk-ui-listener ui-chan))))

(defn stop []
 (when @sys-atom

  (c/stop-system @sys-atom)

  (doseq [c [:com-chan]]
    (close! (get @sys-atom c)))

  (reset! sys-atom nil)) )

(defn start []
  (do
    (stop)
    (->>
      (mk-game config ui-chan)
      (c/start-system ) 
      (reset! sys-atom ))  
    
    (-> @sys-atom :client-connection client/connect!)))

(defn restart []
  (start))

(stop)
(main)






