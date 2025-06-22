(uiop:define-package #:40ants-mcp/server/definition
  (:use #:cl)
  (:import-from #:openrpc-server
                #:api
                #:api-methods)
  (:import-from #:openrpc-server/method
                #:method-info
                #:method-thunk)
  (:import-from #:openrpc-server/discovery
                #:rpc-discover)
  (:import-from #:40ants-mcp/stdio-transport
                #:stdio-transport
                #:send-message
                #:make-response
                #:make-error-response
                #:+mcp-method-not-found+
                #:+mcp-invalid-params+)
  (:import-from #:log)
  (:import-from #:jsonrpc)
  (:import-from #:serapeum
                #:fmt
                #:dict*
                #:->
                #:dict
                #:soft-list-of)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:40ants-mcp/transport/base
                #:start-loop)
  (:export #:mcp-server
           #:start-server
           #:server-tools-collections))
(in-package #:40ants-mcp/server/definition)


(openrpc-server:define-api (mcp-server :title "MCP Server")
  ((tools-collections :initarg collections
                      :initform nil
                      :type (soft-list-of api)
                      :reader server-tools-collections)))


(defun handle-stdio-message (rpc-server message)
  (log:info "Handling" message)
  (restart-case
      (let ((parsed-request
              (handler-case (jsonrpc:parse-message message)
                (jsonrpc:jsonrpc-error ()
                  ;; Nothing can be done
                  nil))))
        (cond
          (parsed-request
           (let ((response (jsonrpc:dispatch rpc-server parsed-request)))
             ;; JSONRPC notifications do not require response,
             ;; in this case we should return NIL:
             (when response
               (with-output-to-string (s)
                 (yason:encode response s)))))
          (t
           (log:error "No message was parsed, TBD: see how to return this error to the caller in MCP protocol.")
           (values NIL))))
    (abort ()
      :report "Abort processing this message and return error to the client"
      (openrpc-server:return-error "Message processing was interrupted."))))


(-> start-server ((or api
                      (soft-list-of api)))
    (values &optional))


(defun start-server (tools-collections)
  "Start the MCP server"
  (log:info "Starting MCP server")

  (let* ((rpc-server (jsonrpc:make-server)))

    (setf (openrpc-server/api::api-server mcp-server)
          rpc-server)

    (setf (slot-value mcp-server 'tools-collections)
          (uiop:ensure-list tools-collections))

    (loop for name being the hash-key of (api-methods mcp-server)
            using (hash-value method-info)
          do (jsonrpc:expose rpc-server name
                             (method-thunk method-info)))

    (start-loop (make-instance 'stdio-transport)
                (lambda (message)
                  (handle-stdio-message rpc-server message)))))


