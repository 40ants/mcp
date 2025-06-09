(defsystem "40ants-mcp-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/mcp/"
  :class :package-inferred-system
  :description "Provides documentation for 40ants-mcp."
  :source-control (:git "https://github.com/40ants/mcp")
  :bug-tracker "https://github.com/40ants/mcp/issues"
  :pathname "docs"
  :depends-on ("40ants-mcp"
               "40ants-mcp-docs/index"))
