(ns client.audio
  (:require

    [taoensso.timbre :refer-macros [log  trace  debug  info ] :as t]

    [goog.dom :as gdom]

    [com.stuartsierra.component :as c]

    [cljs.core.async :refer [<! ] :as a])
  (:require-macros 
    [servalan.macros :as m :refer [dochan]]
    [cljs.core.async.macros :refer [go go-loop]])
  )

(defprotocol IAudio
  (play-track [_ _])
  (stop-track [_ _])
  (stop-all [_ ]))

(defn find-audio []
 (gdom/getElement "audio") )


(defn create-audio [id file]
  (let [e (gdom/createElement "audio")]
    (do
      (->>
        #js {:id id
             :src file
             :loop true }
        (gdom/setProperties e ))
      e)))

(def audio-tracks {:planet-6502 {:name "Planet 6502 "
                                 :author "Gazaxian"
                                 :file "planet_6502.wav" }

                   :ping {:name "ping"
                          :file "ping.wav" }
                   

                   })

(defn add-audio-single! [el id {:keys [file] :as audio}]
  (let [audio-el (create-audio (name  id) (str "audio/" file))]
    (do
      (gdom/appendChild el audio-el)  
      audio-el)))

(defn add-audio! [el audio-tracks]
  (->
    (fn [res k v]
      (assoc res k 
             (add-audio-single! el k v)))
    (reduce-kv {} audio-tracks)))

(defrecord HTMLAudio [audio-objs audio-tracks]

  c/Lifecycle

  (start [this]
    (let [this (c/stop this)
          el (gdom/getElement "audio")
          audio (add-audio! el audio-tracks) ]
      (assoc this :audio-objs audio)))

  (stop [this]
    (when audio-objs
      (stop-all this))
    (assoc this :audio-objs nil))

  IAudio

  (play-track [this track-id]
    (if-let [obj (-> this :audio-objs track-id)]
      (t/info obj)
      (t/error "unable to play" track-id)))

  (stop-track [this au]
    )

  (stop-all [this]
    (doseq [[_ obj] audio-objs]
      (.pause obj)
      (.load obj))))

(defn mk-audio []
  (map->HTMLAudio {:audio-tracks audio-tracks}))

(def a (c/start (mk-audio)))

(defn testo2 []
  (play-track a :planet-6502))


(defn slide-val [secs on-change on-end ]
  (let [hz 60 ]
    (go-loop
      [t 0]
      (<! (a/timeout (/ 1000 hz) ))
      (if (<= t 1)
        (do
          (on-change t)
          (recur (+ t (/ 1 (* secs hz)))))
        (do
          (on-end 1))))))

(defn fade-out-and-stop [el secs]
  (let [v (.-volume el)]
    (slide-val secs
               (fn [t]
                 (set! (.-volume el) (* v (- 1 t))))
               (fn [_]
                 (.load el)
                 (set! (.-volume el) 1)))))

(defn play-and-fade-in [el secs target-v]
  (do
    (set! (.-volume el) 0) 
    (.load el)
    (.play el)
    (slide-val secs
               (fn [t] (set! (.-volume el) t))
               (fn [_] (set! (.-volume el) 1)))))

(def trk (-> a :audio-objs :planet-6502))
(def ping (-> a :audio-objs :ping))







