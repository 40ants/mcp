(defsystem "40ants-mcp-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/mcp/"
  :class :package-inferred-system
  :description "Provides tests for 40ants-mcp."
  :source-control (:git "https://github.com/40ants/mcp")
  :bug-tracker "https://github.com/40ants/mcp/issues"
  :pathname "t"
  :depends-on ("40ants-mcp-tests/core"
               "40ants-mcp-tests/transports/http"
               "rove"
               "lack-test")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
