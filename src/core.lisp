(uiop:define-package #:40ants-mcp/core
  (:use #:cl)
  (:import-from #:openrpc-server
                #:api-methods)
  (:import-from #:openrpc-server/method
                #:method-info
                #:method-thunk)
  (:import-from #:openrpc-server/discovery
                #:rpc-discover)
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
  (:import-from #:openrpc-server/utils
                #:sym-to-api-string)
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

;; (defclass mcp-server ()
;;   ((name :initarg :name
;;          :reader server-name
;;          :initform "40ants-mcp-server"
;;          :documentation "Name of the MCP server")
;;    (version :initarg :version
;;             :reader server-version
;;             :initform "0.1.0"
;;             :documentation "Version of the MCP server")
;;    (capabilities :initarg :capabilities
;;                  :reader server-capabilities
;;                  :initform (alexandria:alist-hash-table
;;                            '(("tools" . t)
;;                              ("resources" . t)
;;                              ("prompts" . t)))
;;                  :documentation "Server capabilities")
;;    (transport :initarg :transport
;;               :reader server-transport
;;               :initform (make-instance 'stdio-transport)
;;               :documentation "Transport layer for communication")
;;    (tools :initform (make-hash-table :test 'equal)
;;           :reader server-tools
;;           :documentation "Registry of available tools")
;;    (resources :initform (make-hash-table :test 'equal)
;;               :reader server-resources
;;               :documentation "Registry of available resources")
;;    (prompts :initform (make-hash-table :test 'equal)
;;             :reader server-prompts
;;             :documentation "Registry of available prompts")
;;    (initialized :initform nil
;;                 :accessor server-initialized-p
;;                 :documentation "Whether the server has been initialized"))
;;   (:documentation "Main MCP server class"))

;; Tool registration

;; (defmethod add-tool ((server mcp-server) name description input-schema function)
;;   "Add a new tool to the server"
;;   (setf (gethash name (server-tools server))
;;         (list :description description
;;               :input-schema input-schema
;;               :function function))
;;   name)

;; Resource registration

;; (defmethod add-resource ((server mcp-server) uri name description mime-type function)
;;   "Add a new resource to the server"
;;   (setf (gethash uri (server-resources server))
;;         (list :name name
;;               :description description
;;               :mime-type mime-type
;;               :function function))
;;   uri)

;; Prompt registration

;; (defmethod add-prompt ((server mcp-server) name description arguments function)
;;   "Add a new prompt to the server"
;;   (setf (gethash name (server-prompts server))
;;         (list :description description
;;               :arguments arguments
;;               :function function))
;;   name)

;; Message handling

;; (defmethod handle-message ((server mcp-server) message)
;;   "Handle incoming JSON-RPC message"

  
  
;;   (let ((method (getf message :|method|))
;;         (id (getf message :|id|))
;;         (params (getf message :|params|)))

;;     (log:info "Handling method: ~A" method)

;;     (cond
;;       ;; Initialize request
;;       ((string= method "initialize")
;;        (handle-initialize server id params))

;;       ;; Tool-related methods
;;       ((string= method "tools/list")
;;        (handle-list-tools server id))
;;       ((string= method "tools/call")
;;        (handle-call-tool server id params))

;;       ;; Resource-related methods
;;       ((string= method "resources/list")
;;        (handle-list-resources server id))
;;       ((string= method "resources/read")
;;        (handle-read-resource server id params))

;;       ;; Prompt-related methods
;;       ((string= method "prompts/list")
;;        (handle-list-prompts server id))
;;       ((string= method "prompts/get")
;;        (handle-get-prompt server id params))

;;       ;; Unknown method
;;       (t
;;        (send-error-response server id +mcp-method-not-found+
;;                             (format nil "Method not found: ~A" method))))))

;; (defmethod handle-initialize ((server mcp-server) id params)
;;   "Handle initialization request"
;;   (declare (ignore params)) ; For now, ignore client capabilities
;;   (setf (server-initialized-p server) t)
;;   (let ((response (make-response
;;                    id
;;                    (alexandria:alist-hash-table
;;                     `(("protocolVersion" . "2024-11-05")
;;                       ("capabilities" . ,(server-capabilities server))
;;                       ("serverInfo" . ,(alexandria:alist-hash-table
;;                                         `(("name" . ,(server-name server))
;;                                           ("version" . ,(server-version server))))))))))
;;     (send-message (server-transport server) response)))

;; (defmethod handle-list-tools ((server mcp-server) id)
;;   "Handle tools/list request"
;;   (unless (server-initialized-p server)
;;     (return-from handle-list-tools
;;       (send-error-response server id -32600 "Server not initialized")))

;;   (let ((tools-list (loop for name being the hash-keys of (server-tools server)
;;                           using (hash-value tool-info)
;;                           collect (alexandria:alist-hash-table
;;                                    `(("name" . ,name)
;;                                      ("description" . ,(getf tool-info :description))
;;                                      ("inputSchema" . ,(getf tool-info :input-schema)))))))
;;     (let ((response (make-response
;;                      id
;;                      (alexandria:alist-hash-table
;;                       `(("tools" . ,tools-list))))))
;;       (send-message (server-transport server) response))))

;; (defmethod handle-call-tool ((server mcp-server) id params)
;;   "Handle tools/call request"
;;   (unless (server-initialized-p server)
;;     (return-from handle-call-tool
;;       (send-error-response server id -32600 "Server not initialized")))

;;   (let ((tool-name (getf params :|name|))
;;         (arguments (getf params :|arguments|)))
;;     (let ((tool-info (gethash tool-name (server-tools server))))
;;       (if tool-info
;;           (handler-case
;;               (let ((result (funcall (getf tool-info :function) arguments)))
;;                 (let ((response (make-response
;;                                  id
;;                                  (alexandria:alist-hash-table
;;                                   `(("content" . ,result)
;;                                     ("isError" . :false))))))
;;                   (send-message (server-transport server) response)))
;;             (error (e)
;;               (send-error-response server id -32603
;;                                    (format nil "Tool execution error: ~A" e))))
;;           (send-error-response server id +mcp-method-not-found+
;;                                (format nil "Tool not found: ~A" tool-name))))))

;; (defmethod handle-list-resources ((server mcp-server) id)
;;   "Handle resources/list request"
;;   (unless (server-initialized-p server)
;;     (return-from handle-list-resources
;;       (send-error-response server id -32600 "Server not initialized")))

;;   (let ((resources-list (loop for uri being the hash-keys of (server-resources server)
;;                               using (hash-value resource-info)
;;                               collect (alexandria:alist-hash-table
;;                                        `(("uri" . ,uri)
;;                                          ("name" . ,(getf resource-info :name))
;;                                          ("description" . ,(getf resource-info :description))
;;                                          ("mimeType" . ,(getf resource-info :mime-type)))))))
;;     (let ((response (make-response
;;                      id
;;                      (alexandria:alist-hash-table
;;                       `(("resources" . ,resources-list))))))
;;       (send-message (server-transport server) response))))

;; (defmethod handle-read-resource ((server mcp-server) id params)
;;   "Handle resources/read request"
;;   (unless (server-initialized-p server)
;;     (return-from handle-read-resource
;;       (send-error-response server id -32600 "Server not initialized")))

;;   (let ((uri (getf params :|uri|)))
;;     (let ((resource-info (gethash uri (server-resources server))))
;;       (if resource-info
;;           (handler-case
;;               (let ((content (funcall (getf resource-info :function) uri)))
;;                 (let ((response (make-response
;;                                  id
;;                                  (alexandria:alist-hash-table
;;                                   `(("contents" . ,content))))))
;;                   (send-message (server-transport server) response)))
;;             (error (e)
;;               (send-error-response server id -32603
;;                                    (format nil "Resource read error: ~A" e))))
;;           (send-error-response server id +mcp-method-not-found+
;;                                (format nil "Resource not found: ~A" uri))))))

;; (defmethod handle-list-prompts ((server mcp-server) id)
;;   "Handle prompts/list request"
;;   (unless (server-initialized-p server)
;;     (return-from handle-list-prompts
;;       (send-error-response server id -32600 "Server not initialized")))

;;   (let ((prompts-list (loop for name being the hash-keys of (server-prompts server)
;;                             using (hash-value prompt-info)
;;                             collect (alexandria:alist-hash-table
;;                                      `(("name" . ,name)
;;                                        ("description" . ,(getf prompt-info :description))
;;                                        ("arguments" . ,(getf prompt-info :arguments)))))))
;;     (let ((response (make-response
;;                      id
;;                      (alexandria:alist-hash-table
;;                       `(("prompts" . ,prompts-list))))))
;;       (send-message (server-transport server) response))))

;; (defmethod handle-get-prompt ((server mcp-server) id params)
;;   "Handle prompts/get request"
;;   (unless (server-initialized-p server)
;;     (return-from handle-get-prompt
;;       (send-error-response server id -32600 "Server not initialized")))

;;   (let ((prompt-name (getf params :|name|))
;;         (arguments (getf params :|arguments|)))
;;     (let ((prompt-info (gethash prompt-name (server-prompts server))))
;;       (if prompt-info
;;           (handler-case
;;               (let ((result (funcall (getf prompt-info :function) arguments)))
;;                 (let ((response (make-response
;;                                  id
;;                                  result)))
;;                   (send-message (server-transport server) response)))
;;             (error (e)
;;               (send-error-response server id -32603
;;                                    (format nil "Prompt execution error: ~A" e))))
;;           (send-error-response server id +mcp-method-not-found+
;;                                (format nil "Prompt not found: ~A" prompt-name))))))

;; (defmethod send-error-response ((server mcp-server) id code message)
;;   "Send an error response"
;;   (let ((response (make-error-response id code message)))
;;     (send-message (server-transport server) response)))

;; Server lifecycle

(openrpc-server:define-api (mcp-server :title "MCP Server"))


;; (defmethod handle-initialize ((server mcp-server) id params)
;;   "Handle initialization request"
;;   (declare (ignore params)) ; For now, ignore client capabilities
;;   (setf (server-initialized-p server) t)
;;   (let ((response (make-response
;;                    id
;;                    (alexandria:alist-hash-table
;;                     `(("protocolVersion" . "2024-11-05")
;;                       ("capabilities" . ,(server-capabilities server))
;;                       ("serverInfo" . ,(alexandria:alist-hash-table
;;                                         `(("name" . ,(server-name server))
;;                                           ("version" . ,(server-version server))))))))))
;;     (send-message (server-transport server) response)))


(defclass initialize-response ()
  ((|protocolVersion| :type string
                      :initform "2025-03-26")
   (capabilities :type hash-table
                 :initform (dict "tools" (dict "listChanged" t)))
   (|serverInfo| :type hash-table
                 :initform (dict "name" "40ants-mcp"
                                 "version" "0.1.0"))))


(openrpc-server:define-rpc-method (mcp-server initialize) (|protocolVersion| capabilities |clientInfo|)
  (:summary "Initialize method.")
  (:description "Called when MCP client wants to know about server's capabilities.")
  (:param |protocolVersion| string "A version of MCP protocol, supported by client.")
  (:param capabilities hash-table "A map of capabilities supported by client.")
  (:param |clientInfo| hash-table "A map describing a client version.")
  (:result initialize-response)
  (log:info "INIT was called" |protocolVersion| capabilities |clientInfo|)
  (make-instance 'initialize-response))


(defclass input-schema ()
  ((type :type string
         :initform "object")
   (properties :type hash-table
               :initarg :properties)
   (required :type (soft-list-of string)
             :initarg :required
             :initform nil)))


(defclass tool-description ()
  ((name :type string
         :initarg :name)
   (description :type string
                :initarg :description)
   (|inputSchema| :type input-schema
                  :initarg :input-schema)))


(defclass tools-list-response ()
  ((tools :type (soft-list-of tool-description)
          :initarg :tools)))

(defclass tools-list-response-with-cursor (tools-list-response)
  ((|nextCursor| :type (or null string)
                 :initform nil
                 :initarg :cursor)))


(defclass content ()
  ((type :type string
         :initform "unknown"
         :initarg :type
         :reader content-type)))


(defmethod print-object ((obj content) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "type: ~A"
            (content-type obj))))


(defclass text-content (content)
  ((text :type string
         :initarg :text
         :reader content-text))
  (:default-initargs :type "text"))


(defmethod print-object ((obj text-content) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "type: ~A, text: ~A"
            (content-type obj)
            (content-text obj))))


(defclass tool-call-response ()
  ((content :type (soft-list-of content)
            :initarg :content)
   (|isError| :type boolean
              :initform nil
              :initarg :is-error)))


(define-condition tool-error ()
  ((content :initarg :content
            :type (or content
                      (soft-list-of content))
            :reader tool-error-content))
  (:report (lambda (condition stream)
             (format stream "~A"
                     (tool-error-content condition)))))


(openrpc-server:define-api (example-tools :title "Example tools"))


(defparameter *mcp-tools*
  (list example-tools))



(openrpc-server:define-rpc-method (example-tools eval-lisp-form) (form &key (in-package "CL-USER"))
  (:summary "Evaluates a given Lisp form and returns a list of values.

             Only one lisp form should be provided as the input.
             If you need to eval a multiple forms, wrap them into
             a PROGN or a similar form.

             A multiple values can be returned. Each value is printed in it's own
             section with a title like VALUE-1, VALUE-2 and so on.

             Also this tool returns STDOUT and STDERR if something was written to these streams.

             In case of an error, the ERROR result with a backtrace will be returned.

             If you need to evaluate form in context of some package other than CL-USER,
             then pass package name in IN-PACKAGE argument.
             All FORM symbols without package qualifier, will be interned into this package.")
  (:param form string "Lisp form to be evaluated, in the s-expression syntax.")
  (:param in-package string "Common Lisp package name to evaluate form in.")
  (:result (soft-list-of text-content))

  (block func
    (with-output-to-string (*standard-output*)
      (with-output-to-string (*error-output*)
        (flet ((make-output-results ()
                 (let ((stdout (str:trim (get-output-stream-string *standard-output*)))
                       (stderr (str:trim (get-output-stream-string *error-output*))))
                   (append (unless (str:emptyp stdout)
                             (list (make-instance 'text-content
                                                  :text (fmt "## STDOUT~2%~A"
                                                             stdout))))
                           (unless (str:emptyp stderr)
                             (list (make-instance 'text-content
                                                  :text (fmt "## STDERR~2%~A"
                                                             stderr))))))))
          (let* ((result-values
                   (multiple-value-list
                    (handler-bind ((serious-condition
                                     (lambda (c)
                                       (let ((error-message
                                               (with-output-to-string (s)
                                                 (format s "## ERROR~2%")
                                                 (trivial-backtrace:print-condition c s))))
                                         (error 'tool-error
                                                :content (list* (make-instance 'text-content
                                                                               :text error-message)
                                                                (make-output-results)))))))
                      (let* ((*package* (or (find-package in-package)
                                            (find-package (string-upcase in-package))
                                            (error 'tool-error
                                                   :content (list* (make-instance 'text-content
                                                                                  :text (fmt "Package \"~A\" was not found."
                                                                                             in-package))))))
                             (package-name (package-name *package*))
                             (forms (uiop:with-safe-io-syntax (:package package-name)
                                      (with-input-from-string (s form)
                                        (uiop:slurp-stream-forms s))))
                             ;; To allow eval multiple forms, we need to wrap
                             ;; them with PROGN:
                             (expression
                               (list* 'progn
                                      forms)))
                        (eval expression))))))

            (return-from func
              (append
               (loop for value in result-values
                     for idx upfrom 1
                     collect (make-instance 'text-content
                                            :text (fmt "## VALUE-~A~2%~A"
                                                       idx
                                                       value)))
               (make-output-results)))))))))



(-> get-method-params (method-info)
    (values hash-table
            list
            &optional))


(defun get-method-params (method-info)
  "Returns a hash-table describing methods and their type schemas and a list of required params."
  (loop with results = (dict)
        with required = nil
        for param in (openrpc-server/method::method-params method-info)
        for param-name = (sym-to-api-string (openrpc-server/method::parameter-name param))
        for param-summary = (openrpc-server/method::parameter-summary param)
        for param-type = (openrpc-server/method::parameter-type param)
        for param-required-p = (openrpc-server/method::parameter-required param)
        for schema = (openrpc-server:type-to-schema param-type)
        do (setf (gethash param-name results)
                 (dict* schema
                        "description" param-summary))
        when param-required-p
          do (push param-name required)
        finally (return (values results
                                required))))


(-> make-tool-descriptions-for-method (string method-info)
    tool-description)

(defun make-tool-description-for-method (method-name method-info)
  (multiple-value-bind (params required)
      (get-method-params method-info)
    (make-instance
     'tool-description
     :name method-name
     :description "Evaluates given Lisp form and returns from a list of values."
     :input-schema (make-instance
                    'input-schema
                    :properties params
                    :required required))))


(-> make-tool-descriptions (openrpc-server/api:api)
    (soft-list-of tool-description))

(defun make-tool-descriptions (rpc-server)
  (loop for name being the hash-key of (api-methods rpc-server)
            using (hash-value method-info)
        collect (make-tool-description-for-method name
                                                  method-info)))


(-> search-tool (string (soft-list-of openrpc-server:api))
    (values (or null
                method-info)))

(defun search-tool (tool-name tool-collections)
  (loop for collection in tool-collections
        for methods = (api-methods collection)
        for method = (gethash tool-name methods)
        thereis method))


(openrpc-server:define-rpc-method (mcp-server tools/call) (name arguments)
  (:summary "A method for calling an server tool with given NAME")
  (:description "Called when MCP client wants do something using a tool.")
  (:param name string "A tool name")
  (:param arguments hash-table  "Arguments of a tool.")
  (:result tool-call-response)
  (log:info "Called tool" name)
  
  (let ((tool (search-tool name *mcp-tools*)))
    (cond
      (tool
       (handler-case
           (let* ((thunk (method-thunk tool))
                  ;; OpenRPC's internals will care about arguments parsing:
                  (result (funcall thunk arguments)))
             (dict "content"
                   ;; In the result THUNK returns objects already serialized
                   ;; into the hash-tables:
                   (uiop:ensure-list result)
                   "isError"
                   yason:false))
         (tool-error (condition)
           (make-instance 'tool-call-response
                          :is-error t
                          :content (uiop:ensure-list
                                    (tool-error-content condition))))))
      (t
       (make-instance 'tool-call-response
                      :is-error t
                      :content (list
                                (make-instance 'text-content
                                               :text (fmt "Tool \"~A\" does not exist."
                                                          name))))))))


(openrpc-server:define-rpc-method (mcp-server tools/list) (&key cursor)
  (:summary "A method returning supported tools list.")
  (:description "Called when MCP client wants to know about server's tools.")
  (:param cursor (or null string) "An optional pagination cursor")
  (:result (or tools-list-response
               tools-list-response-with-cursor))
  ;; See the description and examples in the documentation:
  ;; https://modelcontextprotocol.io/docs/concepts/tools#python
  (log:info "TOOLS/LIST was called" cursor)
  (let ((tools
          (mapcan #'make-tool-descriptions
                  *mcp-tools*)))
    (make-instance 'tools-list-response
                   :tools tools)))


(defun handle-stdio-message (rpc-server message)
  (log:info "Handling" message)
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
       (values NIL)))))


(defun start-server ()
  "Start the MCP server"
  (log:info "Starting MCP server")
  
  (let* ((api mcp-server)
         (rpc-server (jsonrpc:make-server)))
    
    (setf (openrpc-server/api::api-server api)
          rpc-server)

    (loop for name being the hash-key of (api-methods api)
            using (hash-value method-info)
          do (jsonrpc:expose rpc-server name
                             (method-thunk method-info)))
    
    (start-stdio-loop (make-instance 'stdio-transport)
                      (lambda (message)
                        (handle-stdio-message rpc-server message)))))


(defmethod stop-server ((server mcp-server))
  "Stop the MCP server"
  (log:info "Stopping MCP server")
  (error "Not implemented yet")
  ;; (stop-stdio-loop (server-transport server))
  )


;; Convenience function for creating and starting a server

(defun make-mcp-server (&key (name "40ants-mcp-server") (version "0.1.0"))
  "Create a new MCP server instance"
  (make-instance 'mcp-server :name name :version version))
