(ns client.protocols)

(defprotocol ITimer
  (now [_])
  (from-seconds [_ s])
  (tick! [_]))

(defprotocol IRender
  (dims [_])
  (resize! [_ dims])
  (reset-transform! [this])
  (clear-all! [this col])
  (square! [this xy wh col]))

(defprotocol IEvents
  (anim-ch [_])
  (events-ch [this]))

(defprotocol ILog
  (log [this m ]))

(defprotocol IService
  (start! [_])
  (stop! [_])
  (running? [_])
  (state [_]))

(defprotocol IGameLoop
  
  )