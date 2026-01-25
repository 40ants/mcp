(uiop:define-package #:40ants-mcp/server/api/tools/call
  (:use #:cl)
  (:import-from #:openrpc-server
                #:api-methods)
  (:import-from #:openrpc-server/method
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
  (:import-from #:40ants-mcp/server/definition
                #:server-tools-collections
                #:mcp-server)
  (:import-from #:40ants-mcp/server/errors
                #:tool-error-content
                #:tool-error)
  (:import-from #:40ants-mcp/content/base
                #:content))
(in-package #:40ants-mcp/server/api/tools/call)


(defclass tool-call-response ()
  ((content :type (soft-list-of content)
            :initarg :content)
   (|isError| :type boolean
              :initform nil
              :initarg :is-error)))


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
  (log:debug "Called tool" name)
  
  (let ((tool (search-tool name (server-tools-collections mcp-server))))
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
