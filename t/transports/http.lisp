(uiop:define-package #:40ants-mcp-tests/transports/http
  (:use #:cl
        #:rove)
  (:import-from #:40ants-mcp/http-transport
                #:http-transport
                #:transport-app)
  (:import-from #:40ants-mcp/content/text
                #:text-content)
  (:import-from #:40ants-mcp/transport/base
                #:start-loop
                #:stop-loop)
  (:import-from #:yason)
  (:import-from #:lack.test)
  (:import-from #:lack.request)
  (:import-from #:lack.response))
(in-package #:40ants-mcp-tests/transports/http)


(defun generate-env (path &key (method :get) content headers)
  "Generate a Lack environment for testing.
   Similar to lack.test:generate-env but with JSON-RPC specific additions."
  (let ((env (lack.test:generate-env path
                                    :method method
                                    :content content
                                    :headers headers)))
    ;; Add the content to the request parameters
    (when content
      (setf (getf env :content-length) (length content))
      (setf (getf env :raw-body) (babel:string-to-octets content))
      (setf (getf env :content-type) "application/json"))
    env))


(defun make-json-rpc-request (method params &key id)
  "Create a JSON-RPC request object"
  (yason:with-output-to-string* ()
    (yason:encode
     (alexandria:alist-hash-table
      `(("jsonrpc" . "2.0")
        ("method" . ,method)
        ("params" . ,params)
        ,@(when id
            `(("id" . ,id))))
      :test 'equal))))


(defun handle-message (message)
  "Handle a JSON-RPC message and return a response."
  (log:info "Message handler received:" message)
  (handler-case
      (let* ((request (yason:parse message))
             (id (gethash "id" request))
             (method (gethash "method" request))
             (params (gethash "params" request)))
        (log:info "Parsed request:" request)
        (log:info "Method:" method)
        (log:info "Params:" params)
        (log:info "ID:" id)
        (cond
          ((string= method "echo")
           (let ((text (gethash "text" params)))
             (log:info "Echo text:" text)
             (if id
                 (progn
                   (log:info "Creating text content response")
                   (let ((response (make-instance 'text-content
                                                :text text)))
                     (log:info "Created response:" response)
                     response))
                 (progn
                   (log:info "Notification, returning nil")
                   nil))))
          (t
           (log:info "Unknown method, returning error")
           (make-instance 'text-content
                         :text "Unknown method"))))
    (error (e)
      (log:error "Error in message handler:" e)
      (make-instance 'text-content
                    :text (format nil "Error: ~A" e)))))


(deftest http-transport-test
  (testing "Basic HTTP transport functionality"
    (let ((transport (make-instance 'http-transport :port 8081))
          (message-handler #'handle-message))
      (start-loop transport message-handler)
      (unwind-protect
           (progn
             (testing "POST to /mcp endpoint"
               (let* ((request (make-json-rpc-request "echo"
                                                     (alexandria:alist-hash-table
                                                      '(("text" . "Hello World!"))
                                                      :test 'equal)
                                                     :id 1))
                      (response (funcall (transport-app transport)
                                       (generate-env "/mcp"
                                                    :method :post
                                                    :content request
                                                    :headers '(("content-type" . "application/json")
                                                             ("mcp-protocol-version" . "2025-06-18"))))))
                 (ok (= 200 (lack.response:response-status response))
                     "Response status should be 200")
                 (ok (equal "application/json"
                          (getf (lack.response:response-headers response) :content-type))
                     "Response should have JSON content type")))

             (testing "GET to /mcp endpoint should return 404"
               (let ((response (funcall (transport-app transport)
                                      (generate-env "/mcp"))))
                 (ok (= 404 (lack.response:response-status response))
                     "GET requests should return 404")))

             (testing "POST to wrong endpoint should return 404"
               (let ((response (funcall (transport-app transport)
                                      (generate-env "/wrong-path"
                                                   :method :post))))
                 (ok (= 404 (lack.response:response-status response))
                     "Wrong paths should return 404")))

             (testing "Invalid JSON should return error"
               (let ((response (funcall (transport-app transport)
                                      (generate-env "/mcp"
                                                   :method :post
                                                   :content "invalid json"
                                                   :headers '(("content-type" . "application/json")
                                                            ("mcp-protocol-version" . "2025-06-18"))))))
                 (ok (= 400 (lack.response:response-status response))
                     "Invalid JSON should return 400 Bad Request"))))
        (stop-loop transport)))))


(deftest http-transport-notifications-test
  (testing "JSON-RPC notification handling"
    (let ((transport (make-instance 'http-transport :port 8082))
          (message-handler #'handle-message))
      (start-loop transport message-handler)
      (unwind-protect
           (testing "Notification should return 202 Accepted"
             (let* ((request (make-json-rpc-request "echo"
                                                   (alexandria:alist-hash-table
                                                    '(("text" . "Hello World!"))
                                                    :test 'equal)))
                    (response (funcall (transport-app transport)
                                     (generate-env "/mcp"
                                                  :method :post
                                                  :content request
                                                  :headers '(("content-type" . "application/json")
                                                           ("mcp-protocol-version" . "2025-06-18"))))))
               (ok (= 202 (lack.response:response-status response))
                   "Notifications should return 202 Accepted")))
        (stop-loop transport)))))
