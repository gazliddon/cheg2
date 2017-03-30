(ns servalan.protocols.clientactions
  )

(defprotocol IClientActions

  (none [this payload])

  (is-connecting [this payload])
  (has-connected [this payload])

  (is-disconnecting [this payload])
  (has-disconnected [this payload])

  (handling-client-msg [this payload])
  (handling-server-msg [this payload]))
