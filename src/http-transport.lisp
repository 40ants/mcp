(uiop:define-package #:40ants-mcp/http-transport
  (:use #:cl)
  (:import-from #:yason)
  (:import-from #:alexandria)
  (:import-from #:log)
  (:import-from #:lack)
  (:import-from #:sse-server)
  (:import-from #:clack-sse)
  (:import-from #:lack.request
                #:make-request
                #:request-body-parameter)
  (:import-from #:lack.response)
  (:import-from #:40ants-mcp/server/connections)
  (:import-from #:40ants-mcp/transport/base
                #:send-message
                #:receive-message
                #:stop-loop
                #:start-loop)
  (:import-from #:40ants-mcp/content/text
                #:content-text
                #:text-content
                #:text)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:serapeum
                #:dict)
  (:export #:http-transport
           #:transport-port))
(in-package #:40ants-mcp/http-transport)


(defparameter *protocol-version*
  "2025-03-26")


(defclass http-transport ()
  ((port :initarg :port
         :initform 8080
         :reader transport-port
         :documentation "Port number to listen on.")
   (app :accessor transport-lack-app
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


(defmethod initialize-instance :after ((obj http-transport) &rest initargs)
  (declare (ignore initargs))

  ;; Create Lack app with our request handler
  (setf (transport-lack-app obj)
        (lambda (env)
          (let ((response (handle-request obj env)))
            (log:info "Returning" response)
            (values response))))
  (values))




(defun sse-stream-writer (env stream)
  (declare (ignore env))

  (40ants-mcp/server/connections::add-client-connection stream)

  ;; Just sleep to prevent stream closing
  (loop do
    (sleep 10)))


(defparameter *sse-handler*
  (clack-sse:serve-sse 'sse-stream-writer))


(defun handle-request (transport env)
  "Handle an incoming HTTP request."
  (let ((path-info (getf env :path-info))
        (method (getf env :request-method)))
    (flet ((return-error-response (&key (code 500) (message "Internal Server Error"))
             (let ((error-response (dict
                                    "jsonrpc" "2.0"
                                    "error" (dict "code" code
                                                  "message" message))))
               (return-from handle-request
                 (list 500
                       (list :content-type "application/json"
                             :mcp-protocol-version *protocol-version*)
                       (list (with-output-to-string (s)
                               (yason:encode error-response s))))))))
      (cond
        ((and (eq method :GET)
              (string= path-info "/mcp"))
         (log:info "Responding with event stream" method path-info)
         
         (funcall *sse-handler* env))
        
        ;; Only handle POST requests to /mcp endpoint
        ((and (eq method :POST)
              (string= path-info "/mcp"))
         (handler-bind ((type-error (lambda (e)
                                      (log:error "Invalid JSON:" e)
                                      (let ((error-response (alexandria:alist-hash-table
                                                             `(("jsonrpc" . "2.0")
                                                               ("error" . ,(alexandria:alist-hash-table
                                                                            `(("code" . 400)
                                                                              ("message" . "Invalid JSON"))
                                                                            :test 'equal)))
                                                             :test 'equal)))
                                        (return-from handle-request
                                          (list 400
                                                (list :content-type "application/json"
                                                      :mcp-protocol-version *protocol-version*)
                                                (list (with-output-to-string (s)
                                                        (yason:encode error-response s))))))
                                      ))
                        (error (lambda (e)
                                 (log:error "Error processing request:" e)
                                 (return-error-response))))
          
           (let* ((request (lack/request:make-request env))
                  (raw-body (lack/request:request-content request))
                  (body-string (when raw-body
                                 (babel:octets-to-string raw-body)))
                  (body (yason:parse body-string))
                  (id (and body (gethash "id" body))))
             (log:info "Processing request, id:" id ", body string:" body-string)
             (handler-bind ((error (lambda (e)
                                     (log:error "Error in message handler:" e)
                                     
                                     (let ((error-response (alexandria:alist-hash-table
                                                            `(("jsonrpc" . "2.0")
                                                              ("error" . ,(alexandria:alist-hash-table
                                                                           `(("code" . 500)
                                                                             ("message" . "Internal Server Error"))
                                                                           :test 'equal)))
                                                            :test 'equal)))
                                       (return-from handle-request
                                         (list 500
                                               (list :content-type "application/json"
                                                     :mcp-protocol-version *protocol-version*)
                                               (list (with-output-to-string (s)
                                                       (yason:encode error-response s)))))))))
               (with-log-unhandled ()
                 (let ((response (funcall (transport-message-handler transport)
                                          body-string)))
                   (log:info "Handler response:" response)
                   (log:info "Handler response type:" (type-of response))
                   (cond
                     ;; For notifications (no id), return 202 Accepted
                     ((null id)
                      (log:info "Handling notification")
                      (list 202
                            (list :content-type "application/json"
                                  :mcp-protocol-version *protocol-version*)
                            nil))

                     ;; For other responses
                     ((typep response 'string)
                      (log:info "Handling other response")
                      (list 200
                            (list :content-type "application/json"
                                  :mcp-protocol-version *protocol-version*)
                            (list response)))
                     (t
                      (log:info "Unknown response type")
                      (return-error-response)))))))))
        ;; Return 404 for unknown paths
        (t
         (log:warn "Route not found" method path-info)
         (list 404
               (list :content-type "text/plain")
               '("Not Found")))))))


(defmethod start-loop ((transport http-transport) message-handler)
  "Start the HTTP server and begin processing requests."
  (log:info "Starting HTTP transport on port" (transport-port transport))
  (setf (transport-message-handler transport) message-handler)
  
  ;; Start the server
  (setf (transport-server transport)
        (clack:clackup (transport-lack-app transport)
                       :server :hunchentoot
                       :port (transport-port transport)
                       :use-thread nil)))


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
