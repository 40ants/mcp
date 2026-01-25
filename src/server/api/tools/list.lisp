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
         :initform "object"
         :documentation "The JSON Schema type, always 'object' for tool input schemas.")
   (properties :type hash-table
               :initarg :properties
               :documentation "Hash table mapping parameter names to their JSON Schema definitions.")
   (required :type (soft-list-of string)
             :initarg :required
             :initform nil
             :documentation "List of required parameter names."))
  (:documentation "Represents the JSON Schema for a tool's input parameters."))


(defclass tool-description ()
  ((name :type string
         :initarg :name
         :documentation "The unique name of the tool.")
   (description :type string
                :initarg :description
                :documentation "Human-readable description of what the tool does.")
   (|inputSchema| :type input-schema
                  :initarg :input-schema
                  :documentation "JSON Schema describing the tool's input parameters."))
  (:documentation "Describes a tool available in the MCP server.
                  Tools are functions that can be called by the client to perform specific tasks."))


(defclass tools-list-response ()
  ((tools :type (soft-list-of tool-description)
          :initarg :tools
          :documentation "List of available tools in the server."))
  (:documentation "Response object for the tools/list RPC method."))


(defclass tools-list-response-with-cursor (tools-list-response)
  ((|nextCursor| :type (or null string)
                 :initform nil
                 :initarg :cursor
                 :documentation "Optional cursor for pagination of large tool lists."))
  (:documentation "Response object for the tools/list RPC method with pagination support."))


(-> get-method-params (method-info)
    (values hash-table
            list
            &optional))


(defun get-method-params (method-info)
  "Extract parameter information from a method-info object.
   Returns two values:
   1. A hash-table mapping parameter names to their JSON Schema definitions
   2. A list of required parameter names"
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
  "Create a tool-description object from a method name and its method-info.
   The tool description includes the method's name, summary, and parameter schema."
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
  "Create tool descriptions for all methods in an RPC server.
   Returns a list of tool-description objects."
  (loop for name being the hash-key of (api-methods rpc-server)
            using (hash-value method-info)
        collect (make-tool-description-for-method name
                                                  method-info)))


(-> search-tool (string (soft-list-of openrpc-server:api))
    (values (or null
                method-info)))

(defun search-tool (tool-name tool-collections)
  "Search for a tool by name across multiple tool collections.
   Returns the method-info for the tool if found, NIL otherwise."
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
  (log:debug "TOOLS/LIST was called" cursor)
  (let ((tools
          (mapcan #'make-tool-descriptions
                  (server-tools-collections mcp-server))))
    (make-instance 'tools-list-response
                   :tools tools)))
