(ns client.protocols)

(defprotocol ITimer
  (now [_])
  (from-seconds [_ s])
  (tick! [_]))

(defprotocol IRender
  (dims [_])
  (set-transform! [_ a b c d e f])
  (resize! [_ dims])
  (reset-transform! [this])
  (clear-all! [this col])
  (spr! [this img src-xywh xy wh])
  (square! [this xy wh col]))

(defprotocol IEvents
  (anim-ch [_]))


