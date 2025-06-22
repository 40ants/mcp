# 40ants-mcp: Model Context Protocol Server for Common Lisp

A comprehensive framework for building [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) servers in Common Lisp. This library provides a complete implementation of the MCP specification with an easy-to-use API for creating servers that can interact with AI assistants like Claude Desktop.

## Features

- ✅ **Full MCP Specification Support**: Complete implementation of MCP protocol version 2024-11-05
- ✅ **STDIO Transport**: Native support for STDIO-based communication
- ✅ **Tools System**: Register and execute custom tools with JSON Schema validation
- ✅ **Resources System**: Serve dynamic and static resources via URI
- ✅ **Prompts System**: Provide prompt templates with argument interpolation
- ✅ **Built on OpenRPC**: Leverages the robust [40ants OpenRPC library](https://40ants.com/openrpc/)
- ✅ **CLOS-based**: Object-oriented design with proper encapsulation
- ✅ **Easy Integration**: Simple API for adding functionality
- ✅ **Error Handling**: Comprehensive error management with proper JSON-RPC error codes

## Installation

### Prerequisites

- SBCL or another Common Lisp implementation
- Quicklisp
- The [40ants OpenRPC library](https://40ants.com/openrpc/)
- The [40ants-slynk library](https://40ants.com/slynk/) for remote debugging support
- (Optional but recommended) [Roswell](https://github.com/roswell/roswell) for easier script management

### Setup

1. Clone this repository:
```bash
git clone https://github.com/40ants/mcp
cd 40ants-mcp
```

2. (Optional) Install Roswell for the best experience:
```bash
# On macOS with Homebrew
brew install roswell

# On Ubuntu/Debian
curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh

# On other systems, see: https://github.com/roswell/roswell#installation
```

3. Load the system in your Lisp REPL:
```lisp
(ql:quickload :40ants-mcp)
```

## Quick Start

### Basic Server

```lisp
(defpackage #:my-mcp-server
  (:use #:cl)
  (:import-from #:40ants-mcp/core
                #:mcp-server
                #:add-tool
                #:start-server))

(defun create-my-server ()
  (let ((server (make-instance 'mcp-server
                               :name "My MCP Server"
                               :version "1.0.0")))
    
    ;; Add a simple tool
    (add-tool server
              "greet"
              "Generate a greeting message"
              ;; JSON Schema for input validation
              '(("type" . "object")
                ("properties" . (("name" . (("type" . "string")))))
                ("required" . ("name")))
              ;; Tool implementation function
              (lambda (args)
                (let ((name (getf args :|name|)))
                  (list `(("type" . "text")
                         ("text" . ,(format nil "Hello, ~A!" name)))))))
    
    server))

;; Start the server
(start-server (create-my-server))
```

### Advanced Example

See `examples/simple-server.lisp` for a comprehensive example that includes:

- Multiple tools with different input schemas
- Resource serving (both static and dynamic content)
- Prompt templates with arguments
- Error handling demonstrations

## API Reference

### Core Classes

#### `mcp-server`

The main server class that handles MCP protocol communication.

**Slots:**
- `:name` - Server name (string)
- `:version` - Server version (string) 
- `:capabilities` - Server capabilities (hash table)

**Methods:**
- `add-tool (server name description schema function)` - Register a new tool
- `add-resource (server uri name description mime-type function)` - Register a new resource
- `add-prompt (server name description arguments function)` - Register a new prompt
- `start-server (server)` - Start the server and begin processing requests

### Tool Registration

```lisp
(add-tool server "tool-name" "Description" schema function)
```

- `schema`: JSON Schema object defining input parameters
- `function`: Lambda that receives parsed arguments and returns result

**Tool Function Return Format:**
```lisp
;; Return a list of content items
(list 
  (alexandria:alist-hash-table
    '(("type" . "text")
      ("text" . "Tool result text"))))
```

### Resource Registration

```lisp
(add-resource server "file:///path" "Resource Name" "Description" "text/plain" function)
```

**Resource Function Return Format:**
```lisp
;; Return a list with resource content
(list 
  (alexandria:alist-hash-table
    '(("uri" . "file:///path")
      ("mimeType" . "text/plain")
      ("text" . "Resource content"))))
```

### Prompt Registration

```lisp
(add-prompt server "prompt-name" "Description" arguments-schema function)
```

**Prompt Function Return Format:**
```lisp
;; Return a hash table with description and messages
(alexandria:alist-hash-table
  '(("description" . "Prompt description")
    ("messages" . (list-of-message-objects))))
```

## Running the Example

### Option 1: Traditional SBCL Script

To run the included example server using SBCL:

```bash
./run-example.sh
```

### Option 2: Roswell Script (Recommended)

If you have [Roswell](https://github.com/roswell/roswell) installed, you can run the server as a Roswell script:

```bash
./examples/simple-server.ros
```

Or:

```bash
ros examples/simple-server.ros
```

The Roswell version includes additional features:
- Automatic dependency management via Quicklisp
- Enhanced system information tools
- Roswell environment inspection
- Cross-platform compatibility
- **SLYNK debugging support** via [40ants-slynk](https://40ants.com/slynk/)

Both versions will start a server with demonstration tools, resources, and prompts that you can test with any MCP client.

### Remote Debugging with SLYNK

The Roswell version supports remote debugging via SLYNK. Set the `SLYNK_PORT` environment variable to enable:

```bash
# Start MCP server with SLYNK on port 4005 (localhost only)
SLYNK_PORT=4005 ./examples/simple-server.ros

# Start with SLYNK accessible from any interface
SLYNK_PORT=4005 SLYNK_INTERFACE=0.0.0.0 ./examples/simple-server.ros
```

Then connect from your favorite editor (Emacs with SLY, VSCode with Alive, etc.) to `localhost:4005` for interactive development and debugging.

### Testing with Claude Desktop

To use your MCP server with Claude Desktop, add it to your configuration:

```json
{
  "mcpServers": {
    "my-lisp-server": {
      "command": "/path/to/your/run-example.sh"
    }
  }
}
```

## Protocol Details

### Supported MCP Methods

- `initialize` - Server initialization with capability negotiation
- `tools/list` - List available tools
- `tools/call` - Execute a specific tool
- `resources/list` - List available resources  
- `resources/read` - Read resource content
- `prompts/list` - List available prompts
- `prompts/get` - Get prompt with arguments

### Transport

The server uses STDIO transport as specified by the MCP protocol:
- Receives JSON-RPC requests via stdin
- Sends JSON-RPC responses via stdout  
- Logs debug information to stderr

### Error Handling

The server properly handles and reports errors using standard JSON-RPC error codes:
- `-32700` Parse error
- `-32600` Invalid request
- `-32601` Method not found
- `-32602` Invalid params
- `-32603` Internal error

## Development

### Project Structure

```
src/
├── core.lisp              # Main server implementation
├── stdio-transport.lisp   # STDIO communication layer
└── messages.lisp          # MCP message type definitions

examples/
├── simple-server.lisp     # Example server (traditional)
└── simple-server.ros      # Example server (Roswell script)

tests/
└── (test files)           # Unit tests

plan.md                    # Implementation roadmap
run-example.sh            # Script to run example server (SBCL)
```

### Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

### License

This project is licensed under the Unlicense - see the LICENSE file for details.

## Links

- [Model Context Protocol Specification](https://modelcontextprotocol.io/specification/2024-11-05)
- [40ants OpenRPC Library](https://40ants.com/openrpc/)
- [MCP Official Documentation](https://modelcontextprotocol.io/)
- [Claude Desktop MCP Guide](https://modelcontextprotocol.io/clients/claude-desktop)

## Status

This is an alpha implementation. The core functionality is working, but some advanced features may still be in development. See `plan.md` for the complete roadmap and current progress.
