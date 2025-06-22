(uiop:define-package #:40ants-mcp/transport/base
  (:use #:cl)
  (:export #:start-loop
           #:stop-loop
           #:receive-message
           #:send-message))
(in-package #:40ants-mcp/transport/base)


(defgeneric start-loop (transport message-handler)
  (:documentation "Starts message processing using given transport."))


(defgeneric stop-loop (transport)
  (:documentation "Stops message processing using given transport."))


(defgeneric receive-message (transport)
  (:documentation "Receive a JSON-RPC message, returns a message or NIL."))


(defgeneric send-message (transport message)
  (:documentation "Send a JSON-RPC message, returns no values."))
