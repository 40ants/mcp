(uiop:define-package #:40ants-mcp-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:40ants-mcp-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "STDIO"
                              "HTTP"))
  (0.3.0 2026-01-25
         "* Hide log messages by default, most of them now are written with debug level.")
  (0.2.0 2025-06-29
         "* Implemented streamableHTTP protocol and notifications on changed tools.")
  (0.1.0 2025-06-22
         "* Initial version supporting STDIO and Tools."))
