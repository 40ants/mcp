(uiop:define-package #:40ants-mcp-tests/transports/http
  (:use #:cl
        #:rove)
  (:import-from #:40ants-mcp/http-transport
                #:transport-lack-app
                #:transport-message-handler
                #:make-request-handler
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
  (:import-from #:lack.response)
  (:import-from #:lack/test
                #:generate-env))
(in-package #:40ants-mcp-tests/transports/http)


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

(deftest http-transport-test
  (testing "Basic HTTP transport functionality"
    (let* ((transport (make-instance 'http-transport :port 8081))
           (app (transport-lack-app transport))
           (messages nil))
      (setf (transport-message-handler transport)
            (lambda (message)
              (push message messages)))
      (unwind-protect
           (progn
             (testing "POST to /mcp endpoint"
               (let* ((request (make-json-rpc-request "echo"
                                                      (alexandria:alist-hash-table
                                                       '(("text" . "Hello World!"))
                                                       :test 'equal)
                                                      :id 1))
                      (response (funcall app
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
               (let ((response (funcall app
                                        (generate-env "/mcp"))))
                 (ok (= 404 (lack.response:response-status response))
                     "GET requests should return 404")))

             (testing "POST to wrong endpoint should return 404"
               (let ((response (funcall app
                                        (generate-env "/wrong-path"
                                                      :method :post))))
                 (ok (= 404 (lack.response:response-status response))
                     "Wrong paths should return 404")))

             (testing "Invalid JSON should return error"
               (let ((response (funcall app
                                        (generate-env "/mcp"
                                                      :method :post
                                                      :content "invalid json"
                                                      :headers '(("content-type" . "application/json")
                                                                 ("mcp-protocol-version" . "2025-06-18"))))))
                 (ok (= 400 (lack.response:response-status response))
                     "Invalid JSON should return 400 Bad Request"))))))))


(deftest http-transport-notifications-test
  (testing "JSON-RPC notification handling"
    (let ((transport (make-instance 'http-transport :port 8082))
          (messages nil))

      (setf (transport-message-handler transport)
            (lambda (message)
              (push message messages)))
      
      (unwind-protect
           (testing "Notification should return 202 Accepted"
             (let* ((request (make-json-rpc-request "echo"
                                                    (alexandria:alist-hash-table
                                                     '(("text" . "Hello World!"))
                                                     :test 'equal)))
                    (response (funcall (transport-lack-app transport)
                                       (generate-env "/mcp"
                                                     :method :post
                                                     :content request
                                                     :headers '(("content-type" . "application/json")
                                                                ("mcp-protocol-version" . "2025-06-18"))))))
               (ok (= 202 (lack.response:response-status response))
                   "Notifications should return 202 Accepted")))))))
