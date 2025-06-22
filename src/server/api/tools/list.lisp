(uiop:define-package #:40ants-mcp/server/api/tools/list
  (:use #:cl)
  (:import-from #:openrpc-server
                #:api-methods)
  (:import-from #:openrpc-server/method
                #:method-summary
                #:method-info
                #:method-thunk)
  (:import-from #:openrpc-server/discovery
                #:rpc-discover)
  (:import-from #:log)
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
  (:import-from #:40ants-mcp/server/definition
                #:server-tools-collections
                #:mcp-server))
(in-package #:40ants-mcp/server/api/tools/list)


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
     :description (method-summary method-info)
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
                  (server-tools-collections mcp-server))))
    (make-instance 'tools-list-response
                   :tools tools)))
