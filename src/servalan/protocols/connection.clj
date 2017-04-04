(ns servalan.protocols.connection

  )

(defprotocol IConnection

  (command! [_ msg])
  (close! [_])
  (disconnect! [_])
  (is-connected? [_]))
