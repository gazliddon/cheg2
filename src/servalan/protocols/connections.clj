(ns servalan.protocols.connections
  )

(defprotocol IConnections
  (clean-up! [ths])
  (add!  [this conn])
  (send! [this id msg])
  (broadcast! [this msg])
  (close! [this id])
  (close-all! [this]))


