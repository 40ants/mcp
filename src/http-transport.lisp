(uiop:define-package #:40ants-mcp/http-transport
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:alexandria)
  (:import-from #:log)
  (:import-from #:lack)
  (:import-from #:lack.request
                #:make-request
                #:request-body-parameter)
  (:import-from #:lack.response)
  (:import-from #:40ants-mcp/transport/base
                #:send-message
                #:receive-message
                #:stop-loop
                #:start-loop)
  (:import-from #:40ants-mcp/content/text
                #:text-content
                #:text)
  (:export #:http-transport
           #:transport-port))
(in-package #:40ants-mcp/http-transport)


(defclass http-transport ()
  ((port :initarg :port
         :initform 8080
         :reader transport-port
         :documentation "Port number to listen on.")
   (app :accessor transport-app
        :documentation "Lack application instance")
   (server :accessor transport-server
           :documentation "Clack server instance")
   (message-handler :accessor transport-message-handler
                   :documentation "Function to handle incoming messages")
   (running :initform t
            :accessor transport-running-p
            :documentation "Flag indicating if transport is active"))
  (:documentation "HTTP transport implementation for MCP (Model Context Protocol) communication.
                  This class handles JSON-RPC message exchange via HTTP POST requests."))


(defun read-json-body (env)
  "Read and parse JSON request body from Lack environment."
  (let* ((raw-body (getf env :raw-body))
         (body-string (when raw-body
                       (babel:octets-to-string raw-body))))
    (when body-string
      (handler-case
          (yason:parse body-string)
        (error (e)
          (log:error "Invalid JSON in request body:" e)
          (error 'type-error :expected-type 'string :datum body-string))))))


(defun handle-request (transport env)
  "Handle an incoming HTTP request."
  (let ((path-info (getf env :path-info))
        (method (getf env :request-method)))
    (cond
      ;; Only handle POST requests to /mcp endpoint
      ((and (eq method :POST)
            (string= path-info "/mcp"))
       (handler-case
           (let* ((raw-body (getf env :raw-body))
                  (body-string (when raw-body
                               (babel:octets-to-string raw-body)))
                  (body (read-json-body env))
                  (id (and body (gethash "id" body))))
             (log:info "Processing request - raw body:" raw-body)
             (log:info "Processing request - body string:" body-string)
             (log:info "Processing request - parsed body:" body)
             (log:info "Processing request - id:" id)
             (handler-case
                 (let ((response (funcall (transport-message-handler transport)
                                        body-string)))
                   (log:info "Handler response:" response)
                   (log:info "Handler response type:" (type-of response))
                   (cond
                     ;; For notifications (no id), return 202 Accepted
                     ((null id)
                      (log:info "Handling notification")
                      (lack.response:make-response 202
                                                 (list :content-type "application/json"
                                                       :mcp-protocol-version "2025-06-18")
                                                 nil))
                     ;; For regular requests with text content response
                     ((typep response 'text-content)
                      (log:info "Handling text content response")
                      (let* ((result (alexandria:alist-hash-table
                                    `(("jsonrpc" . "2.0")
                                      ("result" . ,(alexandria:alist-hash-table
                                                   `(("type" . "text")
                                                     ("text" . ,(text response)))
                                                   :test 'equal))
                                      ("id" . ,id))
                                    :test 'equal))
                             (json-string (with-output-to-string (s)
                                          (yason:encode result s))))
                        (log:info "Response data:" result)
                        (log:info "JSON string:" json-string)
                        (lack.response:make-response 200
                                                   (list :content-type "application/json"
                                                         :mcp-protocol-version "2025-06-18")
                                                   (list json-string))))
                     ;; For other responses
                     (t
                      (log:info "Handling other response")
                      (let ((json-string (with-output-to-string (s)
                                         (yason:encode response s))))
                        (lack.response:make-response 200
                                                   (list :content-type "application/json"
                                                         :mcp-protocol-version "2025-06-18")
                                                   (list json-string))))))
               (error (e)
                 (log:error "Error in message handler:" e)
                 (let ((error-response (alexandria:alist-hash-table
                                      `(("jsonrpc" . "2.0")
                                        ("error" . ,(alexandria:alist-hash-table
                                                    `(("code" . 500)
                                                      ("message" . "Internal Server Error"))
                                                    :test 'equal)))
                                      :test 'equal)))
                   (lack.response:make-response 500
                                              (list :content-type "application/json"
                                                    :mcp-protocol-version "2025-06-18")
                                              (list (with-output-to-string (s)
                                                     (yason:encode error-response s))))))))
         (type-error (e)
           (log:error "Invalid JSON:" e)
           (let ((error-response (alexandria:alist-hash-table
                                `(("jsonrpc" . "2.0")
                                  ("error" . ,(alexandria:alist-hash-table
                                              `(("code" . 400)
                                                ("message" . "Invalid JSON"))
                                              :test 'equal)))
                                :test 'equal)))
             (lack.response:make-response 400
                                        (list :content-type "application/json"
                                              :mcp-protocol-version "2025-06-18")
                                        (list (with-output-to-string (s)
                                               (yason:encode error-response s))))))
         (error (e)
           (log:error "Error processing request:" e)
           (let ((error-response (alexandria:alist-hash-table
                                `(("jsonrpc" . "2.0")
                                  ("error" . ,(alexandria:alist-hash-table
                                              `(("code" . 500)
                                                ("message" . "Internal Server Error"))
                                              :test 'equal)))
                                :test 'equal)))
             (lack.response:make-response 500
                                        (list :content-type "application/json"
                                              :mcp-protocol-version "2025-06-18")
                                        (list (with-output-to-string (s)
                                               (yason:encode error-response s))))))))
      ;; Return 404 for unknown paths
      (t
       (lack.response:make-response 404
                                  (list :content-type "text/plain")
                                  '("Not Found"))))))


(defmethod start-loop ((transport http-transport) message-handler)
  "Start the HTTP server and begin processing requests."
  (log:info "Starting HTTP transport on port" (transport-port transport))
  (setf (transport-message-handler transport) message-handler)

  ;; Create Lack app with our request handler
  (setf (transport-app transport)
        (lambda (env)
          (handle-request transport env)))

  (break)
  ;; Start the server
  (setf (transport-server transport)
        (clack:clackup (transport-app transport)
                       :server :hunchentoot
                       :port (transport-port transport)
                       :use-thread t)))


(defmethod stop-loop ((transport http-transport))
  "Stop the HTTP server."
  (log:info "Stopping HTTP transport")
  (when (transport-server transport)
    (clack:stop (transport-server transport))
    (setf (transport-running-p transport) nil)))


(defmethod send-message ((transport http-transport) message)
  "Not used in HTTP transport as responses are sent directly in handle-request."
  (declare (ignore message))
  (values))


(defmethod receive-message ((transport http-transport))
  "Not used in HTTP transport as messages are received via HTTP handlers."
  (values))
