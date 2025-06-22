(uiop:define-package #:40ants-mcp-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:40ants-mcp-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@about
           #:@changelog))
(in-package #:40ants-mcp-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "40ants-mcp-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")

  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "40ants-mcp - The framework for building MCP servers and clients in Common Lisp."
                    :ignore-words ("JSON"
                                   "MCP"
                                   "JSON-RPC"
                                   "STDIO"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "API"
                                   "URL"
                                   "URI"
                                   "RPC"
                                   "GIT"
                                   "AI"
                                   "CLOS"))
  (40ants-mcp system)
  "
[![](https://github-actions.40ants.com/40ants/mcp/matrix.svg?only=ci.run-tests)](https://github.com/40ants/mcp/actions)

![Quicklisp](http://quickdocs.org/badge/40ants-mcp.svg)
"
  (@about section)
  (@installation section)
  (@usage section)
  (@api section))


(defsection-copy @readme @index)


(defsection @about (:title "About")
  """
A comprehensive framework for building [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) servers in Common Lisp. This library provides a complete implementation of the MCP specification with an easy-to-use API for creating servers that can interact with AI assistants like Claude Desktop.

## Features

- ✅ **STDIO Transport**: Native support for STDIO-based communication
- ✅ **Tools System**: Register and execute custom tools with JSON Schema validation
- ✅ **Built on OpenRPC**: Leverages the robust [40ants OpenRPC library](https://40ants.com/openrpc/)
- ✅ **CLOS-based**: Object-oriented design with proper encapsulation
- ✅ **Easy Integration**: Simple API for adding functionality
- ✅ **Error Handling**: Comprehensive error management with proper JSON-RPC error codes

## Roadmap

- 🔄 **Full MCP Specification Support**: Complete implementation of MCP protocol version 2024-11-05
- 🔄 **Resources System**: Serve dynamic and static resources via URI
- 🔄 **Prompts System**: Provide prompt templates with argument interpolation
""")


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :40ants-mcp)
```
""")


(defsection @usage (:title "Usage")
  "
TODO: Write a library description. Put some examples here.
")


(defautodoc @api (:system "40ants-mcp"))
