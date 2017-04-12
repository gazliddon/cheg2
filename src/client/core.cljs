(ns client.core
  (:require
    [client.game :as game]

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

    [servalan.fsm :as fsm]
    [client.protocols :as p] 
    [client.html :refer [mk-html-component mk-html-events-component]]
    [client.utils :refer [ch->coll cos cos01] :as u]

    [client.keys :as k]

    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom] 

    [cljs.core.async :refer [chan <! >! put! close! timeout poll!] :as a]
    
    )

  (:require-macros 
    [servalan.macros :as m :refer [dochan]]
    [cljs.core.async.macros :refer [go go-loop]]))


(declare start stop)


(def config { :conn-config {:url  "ws://localhost:6502"
                            :ping-frequency 1000 }
             :html-id "game" })

(defn mk-bidi-chan []
  (let [reader (chan)
        writer (chan) ]

    (go-loop
      [ ]
      (if-let [m (<! writer)]
        (do
          (>! reader m)
          (recur))))
    
    (su/bidi-ch
      reader
      writer)))

(defn mk-game [config ui-chan ]

  (c/system-map

    :ui-chan ui-chan

    :com-chan (mk-bidi-chan)

    :config config

    :system (mk-html-component (:html-id config))

    :events (mk-html-events-component )

    :client-connection (c/using
                        ( mk-client-component (:conn-config config) )
                        [:com-chan
                         :ui-chan ])

    :game (c/using (game/mk-game)
                          [:com-chan
                           :ui-chan
                           :client-connection
                           :events
                           :system
                           :config])
    )
  )


(defonce sys-atom (atom nil))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def objs (atom []))

(def state (atom {:server-time 0 
                  :conn-status :disconnected
                  :time 0 

                  :pings {:last-ping 0 
                          :last-pong 0    } }))

(enable-console-print!)

(defn on-js-reload []
  ;; optionally touch your app-state to force re-rendering depending on
  ;; your application
   ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


(def app-state (atom {:conn-status :disconnected

                      :game-status {:running false
                                    :state nil
                                    }

                      :game-running false }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmulti reader-fn om/dispatch)

; (defmethod reader-fn :game-satus
;  [{:keys [state query] :as this } k k2]
;  {:value (:game @state)})


(defmethod reader-fn :default
 [{:keys [state]} k _]
  (let [st @state] ;; CACHING!!!
    (if (contains? st k)
      {:value (get st k)})))

(defmulti mutate-fn om/dispatch)

(defmethod mutate-fn `game/start
  [{:keys [state]} _ params]
  {:action (fn[] (swap! state assoc :game-running true))})

(defmethod mutate-fn `game/stop
  [{:keys [state]} _ params]
  {:action (fn [] (swap! state assoc :game-running false)) })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mk-button [this txt transaction action]
  [:button
   {:class  "pure-button pure-button-primary"
    :on-click (fn []
                (action)
                (om/transact! this transaction)) }
   txt]

  ; [:button
  ;  {:class  "pure-button pure-button-primary"

  ;   :on-click (fn []
  ;               (action)
  ;               (om/transact! this transaction)) }
  ;  txt]
  )



(def ui-chan (mk-bidi-chan))

(defui App

  static om/IQuery
  (query [this] '[:game-status])

  Object

  (render [this]
          (let [{:keys [game-status] :as props} (om/props this)
                running (:running game-status) ]
            (html
              [:div
               [:h1 "Cheg"]

               (if running
                 (do
                   (mk-button this "Quit" `[(game/stop)] stop) )

                 (mk-button this "Start" `[(game/start)] start)) ]
              ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def reconciler

  (om/reconciler
    {:state app-state
     :parser (om/parser {:read reader-fn :mutate mutate-fn})}))


(defn main []
  (let [app-el (gdom/getElement "app") ]
    (do
      (om/add-root! reconciler
                    App app-el)

      ;; listen for any msg to the UI
      (go-loop
        []
        (when-let [msg (<! ui-chan)]
          ; (t/info "got a message for the UI " msg)
          ;; do a condition for UI messages
          ;; make ui chan bidirectional-s
          (recur))))

    ))

(defn stop []
 (when @sys-atom

  (c/stop-system @sys-atom)

  (doseq [c [:com-chan :ui-chan]]
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


(stop)
(main)






