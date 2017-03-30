(ns servalan.protocols.clientconnection

  )

(defprotocol IClientConnection
  (connect! [_])
  (state? [_])
  (disconnect! [_]))
