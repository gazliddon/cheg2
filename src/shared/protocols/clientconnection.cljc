(ns shared.protocols.clientconnection
  )

(defprotocol IClientConnection
  (send! [_ _])
  (connect! [_])
  (state? [_])
  (disconnect! [_]))
