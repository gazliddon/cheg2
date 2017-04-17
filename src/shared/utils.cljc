(ns shared.utils
  (:require
    [servalan.fsm :as fsm]

    [taoensso.timbre :as t
     :refer-macros [log  trace  debug  info  warn  error  fatal  report
                    logf tracef debugf infof warnf errorf fatalf reportf
                    spy get-env]]
    #?(:clj
       [clojure.core.async :refer [chan <! >! put!
                                   close! go-loop alts!
                                   sliding-buffer timeout]]
       :cljs
       [cljs.core.async :refer [chan put! close! <! >! alts! sliding-buffer timeout]])

    #?(:clj
       [clojure.core.async.impl.protocols :as p]
       :cljs
       [cljs.core.async.impl.protocols :as p]))

  #?(:cljs
     (:require-macros [cljs.core.async.macros :refer [go-loop]])))

(defn bidi-ch

  "bi directional channel
  read-ch && write-ch are from the client's perspective
  "
  
  [read-ch write-ch & [{:keys [on-close]}]]

  (reify
    p/ReadPort
    (take! [_ handler]
      (p/take! read-ch handler))

    p/WritePort
    (put! [_ msg handler]
      (p/put! write-ch msg handler))

    p/Channel
    (close! [_]
      (p/close! read-ch)
      (p/close! write-ch)
      (when on-close
        (on-close)))))


(defn every-n-millis-ch
  ""
  ([millis message-maker]

   (let [read-ch (chan (sliding-buffer 1))
         write-ch (chan ) 
         ret (bidi-ch read-ch write-ch) ]

     (go-loop []

              (let [[m port] (alts! [(timeout millis) write-ch])]

                (if m
                  (close! ret)

                  (do
                    (put! read-ch (message-maker))
                    (recur)))))
     ret))

  ([millis]
   (every-n-millis-ch millis (constantly :tick))))

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
