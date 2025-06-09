(uiop:define-package #:40ants-mcp/core
  (:use #:cl)
  (:import-from #:40ants-mcp/stdio-transport
                #:stdio-transport
                #:start-stdio-loop
                #:send-message
                #:make-response
                #:make-error-response
                #:+mcp-method-not-found+
                #:+mcp-invalid-params+)
  (:import-from #:40ants-mcp/messages
                #:mcp-initialize-response
                #:server-info
                #:list-tools-response
                #:list-resources-response
                #:list-prompts-response)
  (:import-from #:alexandria)
  (:export #:mcp-server
           #:server-name
           #:server-version
           #:server-capabilities
           #:add-tool
           #:add-resource
           #:add-prompt
           #:start-server
           #:stop-server))
(in-package #:40ants-mcp/core)

(defclass mcp-server ()
  ((name :initarg :name 
         :reader server-name
         :initform "40ants-mcp-server"
         :documentation "Name of the MCP server")
   (version :initarg :version 
            :reader server-version
            :initform "0.1.0"
            :documentation "Version of the MCP server")
   (capabilities :initarg :capabilities 
                 :reader server-capabilities
                 :initform (alexandria:alist-hash-table
                           '(("tools" . t)
                             ("resources" . t)
                             ("prompts" . t)))
                 :documentation "Server capabilities")
   (transport :initarg :transport
              :reader server-transport
              :initform (make-instance 'stdio-transport)
              :documentation "Transport layer for communication")
   (tools :initform (make-hash-table :test 'equal)
          :reader server-tools
          :documentation "Registry of available tools")
   (resources :initform (make-hash-table :test 'equal)
              :reader server-resources
              :documentation "Registry of available resources")
   (prompts :initform (make-hash-table :test 'equal)
            :reader server-prompts
            :documentation "Registry of available prompts")
   (initialized :initform nil
                :accessor server-initialized-p
                :documentation "Whether the server has been initialized"))
  (:documentation "Main MCP server class"))

;; Tool registration

(defmethod add-tool ((server mcp-server) name description input-schema function)
  "Add a new tool to the server"
  (setf (gethash name (server-tools server))
        (list :description description
              :input-schema input-schema
              :function function))
  name)

;; Resource registration

(defmethod add-resource ((server mcp-server) uri name description mime-type function)
  "Add a new resource to the server"
  (setf (gethash uri (server-resources server))
        (list :name name
              :description description
              :mime-type mime-type
              :function function))
  uri)

;; Prompt registration

(defmethod add-prompt ((server mcp-server) name description arguments function)
  "Add a new prompt to the server"
  (setf (gethash name (server-prompts server))
        (list :description description
              :arguments arguments
              :function function))
  name)

;; Message handling

(defmethod handle-message ((server mcp-server) message)
  "Handle incoming JSON-RPC message"
  (let ((method (getf message :|method|))
        (id (getf message :|id|))
        (params (getf message :|params|)))
    
    (format *error-output* "Handling method: ~A~%" method)
    
    (cond
      ;; Initialize request
      ((string= method "initialize")
       (handle-initialize server id params))
      
      ;; Tool-related methods
      ((string= method "tools/list")
       (handle-list-tools server id))
      ((string= method "tools/call")
       (handle-call-tool server id params))
      
      ;; Resource-related methods
      ((string= method "resources/list")
       (handle-list-resources server id))
      ((string= method "resources/read")
       (handle-read-resource server id params))
      
      ;; Prompt-related methods
      ((string= method "prompts/list")
       (handle-list-prompts server id))
      ((string= method "prompts/get")
       (handle-get-prompt server id params))
      
      ;; Unknown method
      (t
       (send-error-response server id +mcp-method-not-found+ 
                            (format nil "Method not found: ~A" method))))))

(defmethod handle-initialize ((server mcp-server) id params)
  "Handle initialization request"
  (declare (ignore params)) ; For now, ignore client capabilities
  (setf (server-initialized-p server) t)
  (let ((response (make-response 
                   id
                   (alexandria:alist-hash-table
                    `(("protocolVersion" . "2024-11-05")
                      ("capabilities" . ,(server-capabilities server))
                      ("serverInfo" . ,(alexandria:alist-hash-table
                                        `(("name" . ,(server-name server))
                                          ("version" . ,(server-version server))))))))))
    (send-message (server-transport server) response)))

(defmethod handle-list-tools ((server mcp-server) id)
  "Handle tools/list request"
  (unless (server-initialized-p server)
    (return-from handle-list-tools
      (send-error-response server id -32600 "Server not initialized")))
  
  (let ((tools-list (loop for name being the hash-keys of (server-tools server)
                          using (hash-value tool-info)
                          collect (alexandria:alist-hash-table
                                   `(("name" . ,name)
                                     ("description" . ,(getf tool-info :description))
                                     ("inputSchema" . ,(getf tool-info :input-schema)))))))
    (let ((response (make-response 
                     id
                     (alexandria:alist-hash-table
                      `(("tools" . ,tools-list))))))
      (send-message (server-transport server) response))))

(defmethod handle-call-tool ((server mcp-server) id params)
  "Handle tools/call request"
  (unless (server-initialized-p server)
    (return-from handle-call-tool
      (send-error-response server id -32600 "Server not initialized")))
  
  (let ((tool-name (getf params :|name|))
        (arguments (getf params :|arguments|)))
    (let ((tool-info (gethash tool-name (server-tools server))))
      (if tool-info
          (handler-case
              (let ((result (funcall (getf tool-info :function) arguments)))
                (let ((response (make-response 
                                 id
                                 (alexandria:alist-hash-table
                                  `(("content" . ,result)
                                    ("isError" . :false))))))
                  (send-message (server-transport server) response)))
            (error (e)
              (send-error-response server id -32603 
                                   (format nil "Tool execution error: ~A" e))))
          (send-error-response server id +mcp-method-not-found+
                               (format nil "Tool not found: ~A" tool-name))))))

(defmethod handle-list-resources ((server mcp-server) id)
  "Handle resources/list request"
  (unless (server-initialized-p server)
    (return-from handle-list-resources
      (send-error-response server id -32600 "Server not initialized")))
  
  (let ((resources-list (loop for uri being the hash-keys of (server-resources server)
                              using (hash-value resource-info)
                              collect (alexandria:alist-hash-table
                                       `(("uri" . ,uri)
                                         ("name" . ,(getf resource-info :name))
                                         ("description" . ,(getf resource-info :description))
                                         ("mimeType" . ,(getf resource-info :mime-type)))))))
    (let ((response (make-response 
                     id
                     (alexandria:alist-hash-table
                      `(("resources" . ,resources-list))))))
      (send-message (server-transport server) response))))

(defmethod handle-read-resource ((server mcp-server) id params)
  "Handle resources/read request"
  (unless (server-initialized-p server)
    (return-from handle-read-resource
      (send-error-response server id -32600 "Server not initialized")))
  
  (let ((uri (getf params :|uri|)))
    (let ((resource-info (gethash uri (server-resources server))))
      (if resource-info
          (handler-case
              (let ((content (funcall (getf resource-info :function) uri)))
                (let ((response (make-response 
                                 id
                                 (alexandria:alist-hash-table
                                  `(("contents" . ,content))))))
                  (send-message (server-transport server) response)))
            (error (e)
              (send-error-response server id -32603 
                                   (format nil "Resource read error: ~A" e))))
          (send-error-response server id +mcp-method-not-found+
                               (format nil "Resource not found: ~A" uri))))))

(defmethod handle-list-prompts ((server mcp-server) id)
  "Handle prompts/list request"
  (unless (server-initialized-p server)
    (return-from handle-list-prompts
      (send-error-response server id -32600 "Server not initialized")))
  
  (let ((prompts-list (loop for name being the hash-keys of (server-prompts server)
                            using (hash-value prompt-info)
                            collect (alexandria:alist-hash-table
                                     `(("name" . ,name)
                                       ("description" . ,(getf prompt-info :description))
                                       ("arguments" . ,(getf prompt-info :arguments)))))))
    (let ((response (make-response 
                     id
                     (alexandria:alist-hash-table
                      `(("prompts" . ,prompts-list))))))
      (send-message (server-transport server) response))))

(defmethod handle-get-prompt ((server mcp-server) id params)
  "Handle prompts/get request"
  (unless (server-initialized-p server)
    (return-from handle-get-prompt
      (send-error-response server id -32600 "Server not initialized")))
  
  (let ((prompt-name (getf params :|name|))
        (arguments (getf params :|arguments|)))
    (let ((prompt-info (gethash prompt-name (server-prompts server))))
      (if prompt-info
          (handler-case
              (let ((result (funcall (getf prompt-info :function) arguments)))
                (let ((response (make-response 
                                 id
                                 result)))
                  (send-message (server-transport server) response)))
            (error (e)
              (send-error-response server id -32603 
                                   (format nil "Prompt execution error: ~A" e))))
          (send-error-response server id +mcp-method-not-found+
                               (format nil "Prompt not found: ~A" prompt-name))))))

(defmethod send-error-response ((server mcp-server) id code message)
  "Send an error response"
  (let ((response (make-error-response id code message)))
    (send-message (server-transport server) response)))

;; Server lifecycle

(defmethod start-server ((server mcp-server))
  "Start the MCP server"
  (format *error-output* "Starting MCP server: ~A v~A~%" 
          (server-name server) (server-version server))
  (start-stdio-loop (server-transport server)
                    (lambda (message)
                      (handle-message server message))))

(defmethod stop-server ((server mcp-server))
  "Stop the MCP server"
  (format *error-output* "Stopping MCP server~%"))

;; Convenience function for creating and starting a server

(defun make-mcp-server (&key (name "40ants-mcp-server") (version "0.1.0"))
  "Create a new MCP server instance"
  (make-instance 'mcp-server :name name :version version))
