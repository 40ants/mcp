(uiop:define-package #:40ants-mcp/server
  (:use #:cl)
  (:import-from #:openrpc-server
                #:api-methods)
  (:import-from #:openrpc-server/method
                #:method-info
                #:method-thunk)
  (:import-from #:openrpc-server/discovery
                #:rpc-discover)
  (:import-from #:40ants-mcp/server/errors
                #:tool-error-content
                #:tool-error)
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
(in-package #:40ants-mcp/server)


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


(defclass tool-call-response ()
  ((content :type (soft-list-of content)
            :initarg :content)
   (|isError| :type boolean
              :initform nil
              :initarg :is-error)))



(defparameter *mcp-tools*
  (list example-tools))



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
