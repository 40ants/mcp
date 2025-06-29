(uiop:define-package #:40ants-mcp/server/connections
  (:use #:cl)
  (:import-from #:sento.actor-system)
  (:import-from #:sse-server))
(in-package #:40ants-mcp/server/connections)


(defvar *agent-system* nil)


(defvar *client-connections* nil)


(defun get-agent-system ()
  (unless *agent-system*
    (setf *agent-system*
          (sento.actor-system:make-actor-system)))
  (values *agent-system*))


(defun process-message (message sse-stream)
  (declare (ignore message))

  (log:info "Sending notification about tools list change")
  
  (sse-server:send-event! sse-stream 
                          "message"
                          "{
  \"jsonrpc\": \"2.0\",
  \"method\": \"notifications/tools/list_changed\"
}")
  
  (finish-output sse-stream))


(defun add-client-connection (sse-stream)
  (let ((agent (sento.actor-context:actor-of (get-agent-system)
                                             :receive (lambda (message)
                                                        (process-message message sse-stream)))))
    (push agent *client-connections*)
    (values)))


(defun notify-all-clients ()
  (loop for client in *client-connections*
        do (sento.actor:ask client "notifications/tools/list_changed")))
