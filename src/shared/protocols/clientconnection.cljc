(ns shared.protocols.clientconnection

  )

(defprotocol IClientConnection
  (connect! [_])
  (state? [_])
  (disconnect! [_]))
