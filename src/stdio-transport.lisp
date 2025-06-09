(uiop:define-package #:40ants-mcp/stdio-transport
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:alexandria)
  (:export #:stdio-transport
           #:transport-input
           #:transport-output
           #:transport-running-p
           #:send-message
           #:receive-message
           #:start-stdio-loop
           #:stop-transport))
(in-package #:40ants-mcp/stdio-transport)

(defclass stdio-transport ()
  ((input-stream :initarg :input-stream 
                 :initform *standard-input*
                 :accessor transport-input
                 :documentation "Input stream for reading JSON-RPC messages")
   (output-stream :initarg :output-stream 
                  :initform *standard-output*
                  :accessor transport-output
                  :documentation "Output stream for writing JSON-RPC responses")
   (running :initform t 
            :accessor transport-running-p
            :documentation "Flag indicating if transport is active"))
  (:documentation "STDIO transport for MCP communication via stdin/stdout"))

(defmethod send-message ((transport stdio-transport) message)
  "Send a JSON-RPC message via stdout as line-delimited JSON"
  (let ((json-string (with-output-to-string (stream)
                       (yason:encode message stream))))
    ;; Write single line JSON without pretty printing
    (write-line json-string (transport-output transport))
    (force-output (transport-output transport))
    ;; Log to stderr for debugging (not part of protocol)
    (format *error-output* "SENT: ~A~%" json-string)))

(defmethod receive-message ((transport stdio-transport))
  "Receive a JSON-RPC message via stdin, returns nil on EOF"
  (handler-case
      (let ((line (read-line (transport-input transport) nil nil)))
        (when line
          ;; Log to stderr for debugging
          (format *error-output* "RECEIVED: ~A~%" line)
          ;; Parse JSON and return as plist for easier handling
          (yason:parse line :object-as :plist)))
    (end-of-file ()
      ;; Client closed stdin - normal shutdown
      (setf (transport-running-p transport) nil)
      nil)
    (error (e)
      ;; Log parsing errors to stderr
      (format *error-output* "ERROR parsing message: ~A~%" e)
      nil)))

(defmethod start-stdio-loop ((transport stdio-transport) message-handler)
  "Start the main STDIO message loop"
  (format *error-output* "Starting STDIO transport loop...~%")
  (loop while (transport-running-p transport)
        for message = (receive-message transport)
        when message
        do (handler-case
               (funcall message-handler message)
             (error (e)
               ;; Log handler errors to stderr
               (format *error-output* "ERROR handling message: ~A~%" e))))
  (format *error-output* "STDIO transport loop ended.~%"))

(defmethod stop-transport ((transport stdio-transport))
  "Stop the transport gracefully"
  (setf (transport-running-p transport) nil))

;; Utility functions for message creation

(defun make-response (id result)
  "Create a JSON-RPC 2.0 response message"
  (alexandria:alist-hash-table
   `(("jsonrpc" . "2.0")
     ("id" . ,id)
     ("result" . ,result))))

(defun make-error-response (id code message &optional data)
  "Create a JSON-RPC 2.0 error response"
  (alexandria:alist-hash-table
   `(("jsonrpc" . "2.0")
     ("id" . ,id)
     ("error" . ,(alexandria:alist-hash-table
                  `(("code" . ,code)
                    ("message" . ,message)
                    ,@(when data `(("data" . ,data)))))))))

(defun make-notification (method params)
  "Create a JSON-RPC 2.0 notification message"
  (alexandria:alist-hash-table
   `(("jsonrpc" . "2.0")
     ("method" . ,method)
     ("params" . ,params))))

;; MCP-specific error codes
(defconstant +mcp-parse-error+ -32700)
(defconstant +mcp-invalid-request+ -32600)
(defconstant +mcp-method-not-found+ -32601)
(defconstant +mcp-invalid-params+ -32602)
(defconstant +mcp-internal-error+ -32603) 