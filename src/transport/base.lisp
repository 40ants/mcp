(uiop:define-package #:40ants-mcp/transport/base
  (:use #:cl)
  (:export #:start-loop
           #:stop-loop))
(in-package #:40ants-mcp/transport/base)


(defgeneric start-loop (transport message-handler)
  (:documentation "Starts message processing using given transport."))


(defgeneric stop-loop (transport)
  (:documentation "Stops message processing using given transport."))
