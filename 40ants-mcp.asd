#-asdf3.1 (error "40ants-mcp requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "40ants-mcp"
  :description "The framework for building MCP servers and clients in Common Lisp."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/mcp/"
  :source-control (:git "https://github.com/40ants/mcp")
  :bug-tracker "https://github.com/40ants/mcp/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("40ants-mcp/core"
               "40ants-mcp/server/api/initialize"
               "40ants-mcp/server/api/tools/list"
               "40ants-mcp/server/api/tools/call"
               "40ants-mcp/content/text"
               "40ants-mcp/http-transport"
               "40ants-mcp/tools"
               "openrpc-server"         ; JSON-RPC foundation
               "openrpc-client"         ; For testing and client features
               "yason"                  ; JSON handling (already used by openrpc)
               "bordeaux-threads"       ; Threading for STDIO transport
               "trivial-gray-streams"   ; For STDIO stream handling
               "alexandria"             ; Utilities
               "local-time"             ; Timestamps
               "uuid"                   ; Request IDs
               "cl-ppcre"               ; Pattern matching
               "lack"                   ; Web application framework
               "lack-request"           ; HTTP request handling
               "lack-response"          ; HTTP response handling
               "clack"                  ; HTTP server
               "log4cl")              ; Logging
  :in-order-to ((test-op (test-op "40ants-mcp-tests"))))


(asdf:register-system-packages "log4cl" '("LOG"))
(asdf:register-system-packages "lack/response" '("LACK.RESPONSE" "LACK.REQUEST"))
(asdf:register-system-packages "lack/test" '("LACK.TEST"))
(asdf:register-system-packages "sento" '("SENTO.ACTOR-SYSTEM"))
