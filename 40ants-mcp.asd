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
  :depends-on ("40ants-mcp/core")
  :in-order-to ((test-op (test-op "40ants-mcp-tests"))))
