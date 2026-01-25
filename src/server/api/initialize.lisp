(uiop:define-package #:40ants-mcp/server/api/initialize
  (:use #:cl)
  (:import-from #:openrpc-server/method
                #:method-info
                #:method-thunk)
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
                #:mcp-server))
(in-package #:40ants-mcp/server/api/initialize)


(defclass initialize-response ()
  ((|protocolVersion| :type string
                      :initform "2025-03-26")
   (capabilities :type hash-table
                 :initform (dict "tools" (dict "listChanged" t)))
   (|serverInfo| :type hash-table
                 :initform (dict "name" "40ants-mcp"
                                 "title" "An example of MCP written in Common Lisp."
                                 "version" "0.1.0"))))


(openrpc-server:define-rpc-method (mcp-server initialize) (|protocolVersion| capabilities |clientInfo|)
  (:summary "Initialize method.")
  (:description "Called when MCP client wants to know about server's capabilities.")
  (:param |protocolVersion| string "A version of MCP protocol, supported by client.")
  (:param capabilities hash-table "A map of capabilities supported by client.")
  (:param |clientInfo| hash-table "A map describing a client version.")
  (:result initialize-response)
  (log:debug "INIT was called" |protocolVersion| capabilities |clientInfo|)
  (make-instance 'initialize-response))
