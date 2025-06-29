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
  (:import-from #:40ants-mcp/http-transport
                #:http-transport)
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


(defun handle-message (rpc-server message)
  "Generic message handler for both STDIO and HTTP transports."
  (log:info "Handling" message)
  (restart-case
      (let ((parsed-request
              (handler-case (jsonrpc:parse-message message)
                (jsonrpc:jsonrpc-error (error)
                  ;; Nothing can be done
                  (return-from handle-message
                    (list 400
                          nil
                          (list (fmt "Unable to decode body: ~A"
                                     error))))))))
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


(-> initialize-rpc-server ((or api
                               (soft-list-of api)))
    (values jsonrpc:server))

(defun initialize-rpc-server (tools-collections)
  "Initializes OpenRPC server and binds it to the "
  (let* ((rpc-server (jsonrpc:make-server)))

    (setf (openrpc-server/api::api-server mcp-server)
          rpc-server)

    (setf (slot-value mcp-server 'tools-collections)
          (uiop:ensure-list tools-collections))

    (loop for name being the hash-key of (api-methods mcp-server)
            using (hash-value method-info)
          do (jsonrpc:expose rpc-server name
                             (method-thunk method-info)))

    (values rpc-server)))


(-> start-server ((or api
                      (soft-list-of api))
                  &key
                  (:transport (member :stdio :http))
                  (:port (or null integer)))
    (values &optional))


(defun start-server (tools-collections &key (transport :stdio) (port 8080))
  "Start the MCP server with specified transport.
   TRANSPORT can be :stdio or :http.
   PORT is only used when transport is :http."
  (log:info "Starting MCP server with" transport "transport")

  (let* ((rpc-server (initialize-rpc-server tools-collections)))
    ;; Create and start transport
    (let ((transport-instance
            (ecase transport
              (:stdio (make-instance 'stdio-transport))
              (:http (make-instance 'http-transport :port port)))))

      (start-loop transport-instance
                  (lambda (message)
                    (handle-message rpc-server message))))))
