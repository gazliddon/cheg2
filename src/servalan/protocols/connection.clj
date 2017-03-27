(ns servalan.protocols.connection

  )

(defprotocol IConnection
  (command! [_ msg])
  (close! [_])
  (is-connected? [_]))
