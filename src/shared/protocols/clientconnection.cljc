(ns shared.protocols.clientconnection
  )

(defprotocol IClientConnection
  (is-connected? [_])
  (connect! [_])
  (send! [_ msg])
  (disconnect! [_]))
