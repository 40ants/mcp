(defsystem "40ants-mcp-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/mcp/"
  :class :package-inferred-system
  :description "Provides CI settings for 40ants-mcp."
  :source-control (:git "https://github.com/40ants/mcp")
  :bug-tracker "https://github.com/40ants/mcp/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "40ants-mcp-ci/ci"))
