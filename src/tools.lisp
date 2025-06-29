(uiop:define-package #:40ants-mcp/tools
  (:use #:cl)
  (:import-from #:40ants-mcp/server/connections)
  (:import-from #:openrpc-server)
  (:export
   #:define-tool))
(in-package #:40ants-mcp/tools)


(defmacro define-tool (name args &body body)
  `(progn
     (openrpc-server:define-rpc-method ,name ,args
       ,@body)
     (when 40ants-mcp/server/connections::*client-connections*
       (40ants-mcp/server/connections::notify-all-clients))))
