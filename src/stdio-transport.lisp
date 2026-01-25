(uiop:define-package #:40ants-mcp/stdio-transport
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:alexandria)
  (:import-from #:log)
  (:import-from #:40ants-mcp/transport/base
                #:send-message
                #:receive-message
                #:stop-loop
                #:start-loop)
  (:export #:stdio-transport
           #:transport-input
           #:transport-output
           #:transport-running-p
           #:send-message
           #:receive-message))
(in-package #:40ants-mcp/stdio-transport)

(defclass stdio-transport ()
  ((input-stream :initarg :input-stream
                 :initform *standard-input*
                 :accessor transport-input
                 :documentation "Input stream for reading JSON-RPC messages. Defaults to *standard-input*.")
   (output-stream :initarg :output-stream
                  :initform *standard-output*
                  :accessor transport-output
                  :documentation "Output stream for writing JSON-RPC responses. Defaults to *standard-output*.")
   (running :initform t
            :accessor transport-running-p
            :documentation "Flag indicating if transport is active and processing messages."))
  (:documentation "STDIO transport implementation for MCP (Model Context Protocol) communication.
                  This class handles JSON-RPC message exchange via standard input/output streams.
                  It is designed to work with the MCP protocol specification for AI model communication."))

(defmethod send-message ((transport stdio-transport) message)
  "Send a JSON-RPC message via stdout as line-delimited JSON.

   MESSAGE should be a data structure that can be serialized to JSON.
   The message is written as a single line without pretty printing and flushed immediately.
   For debugging purposes, sent messages are also logged to stderr."
  (let ((json-string (with-output-to-string (stream)
                       (yason:encode message stream))))
    ;; Write single line JSON without pretty printing
    (write-line json-string (transport-output transport))
    (force-output (transport-output transport))
    ;; Log to stderr for debugging (not part of protocol)
    (log:debug "SENT: ~A" json-string)
    (values)))


;; TODO: remove after debug
(defvar *received-messages* nil)

(defmethod receive-message ((transport stdio-transport))
  "Receive a JSON-RPC message via stdin.

   Returns the raw message line as a string, or NIL if:
   - End of file is reached (client closed stdin)
   - An error occurs during message parsing

   Messages are logged to stderr for debugging purposes."
  (handler-case
      (let ((line (read-line (transport-input transport) nil nil)))
        (when line
          ;; Log to stderr for debugging
          (log:debug "RECEIVED: ~A" line)
          ;; Parse JSON and return as plist for easier handling
          (push line
                *received-messages*)
          (values line)))
    (end-of-file ()
      ;; Client closed stdin - normal shutdown
      (setf (transport-running-p transport) nil)
      nil)
    (error (e)
      ;; Log parsing errors to stderr
      (log:debug "ERROR parsing message: ~A" e)
      nil)))


(defun process-message (message message-handler)
  (handler-case
      (let ((response (funcall message-handler message)))
        (cond
          (response
           (log:debug "Responding with" response)
           (write-string response)
           (terpri)
           (finish-output))
          (t
           (log:debug "There is no response"))))
    (error (e)
      ;; Log handler errors to stderr
      (log:debug "ERROR handling message: ~A" e))))


(defmethod start-loop ((transport stdio-transport) message-handler)
  "Start the main STDIO message processing loop.

   MESSAGE-HANDLER should be a function that takes a message string and returns a response string.
   The loop continues until transport-running-p becomes NIL or EOF is reached on input.
   Each received message is processed by the message-handler and any response is written to output."
  (log:debug "Starting STDIO transport loop...")
  (loop while (transport-running-p transport)
        for message = (receive-message transport)
        do (cond
             (message
              (process-message message message-handler))
             (t
              ;; Without this sleep, transport eats a whole CPU :(
              (sleep 0.5))))
  (log:debug "STDIO transport loop ended."))


(defmethod stop-loop ((transport stdio-transport))
  "Stop the transport gracefully by setting the running flag to NIL.
   This will cause the message loop to terminate after processing any current message."
  (setf (transport-running-p transport) nil))

;; Utility functions for message creation

(defun make-response (id result)
  "Create a JSON-RPC 2.0 response message.

   ID should match the request id.
   RESULT is the data to return to the client."
  (alexandria:alist-hash-table
   `(("jsonrpc" . "2.0")
     ("id" . ,id)
     ("result" . ,result))))

(defun make-error-response (id code message &optional data)
  "Create a JSON-RPC 2.0 error response.

   ID should match the request id.
   CODE should be one of the standard JSON-RPC error codes or MCP-specific codes.
   MESSAGE is a human-readable error description.
   DATA is optional additional error information."
  (alexandria:alist-hash-table
   `(("jsonrpc" . "2.0")
     ("id" . ,id)
     ("error" . ,(alexandria:alist-hash-table
                  `(("code" . ,code)
                    ("message" . ,message)
                    ,@(when data `(("data" . ,data)))))))))

(defun make-notification (method params)
  "Create a JSON-RPC 2.0 notification message.

   METHOD is the name of the notification method.
   PARAMS is the notification parameters object."
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
