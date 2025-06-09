# MCP Server Implementation Plan for Common Lisp

## Project Overview
The goal is to implement a complete Model Context Protocol (MCP) server in Common Lisp using the existing 40ants OpenRPC library as the foundation. The server will communicate over STDIO transport as specified in the Model Context Protocol specification.

## Key Resources
- [MCP Specification](https://modelcontextprotocol.io/specification/2025-03-26)
- [40ants OpenRPC Library](https://40ants.com/openrpc/)
- [MCP Server Examples](https://modelcontextprotocol.io/quickstart/server)

## Architecture Overview

### Foundation: OpenRPC Library
We'll build on the 40ants OpenRPC library which provides:
- Mature JSON-RPC 2.0 implementation
- Automatic CLOS object serialization
- Type system with JSON Schema generation
- Middleware support
- Method dispatch and parameter validation

### STDIO Transport
- Communication via standard input/output streams
- Line-delimited JSON messages
- Process-based integration
- Client spawns server as subprocess

## Dependencies

Update `40ants-mcp.asd`:
```common-lisp
:depends-on ("openrpc-server"     ; JSON-RPC foundation
             "openrpc-client"     ; For testing and client features
             "yason"              ; JSON handling (already used by openrpc)
             "bordeaux-threads"   ; Threading for STDIO transport
             "trivial-gray-streams" ; For STDIO stream handling
             "alexandria"         ; Utilities
             "local-time"         ; Timestamps
             "uuid"               ; Request IDs
             "cl-ppcre"           ; Pattern matching
             "log4cl")            ; Logging
```

## Implementation Phases

### Phase 1: STDIO Transport Layer (Week 1)

#### 1.1 STDIO Transport Core
**File**: `src/stdio-transport.lisp`

```common-lisp
(defclass stdio-transport ()
  ((input-stream :initarg :input-stream 
                 :initform *standard-input*
                 :accessor transport-input)
   (output-stream :initarg :output-stream 
                  :initform *standard-output*
                  :accessor transport-output)
   (running :initform t :accessor transport-running-p)))

(defmethod send-message ((transport stdio-transport) message)
  "Send a JSON-RPC message via stdout"
  (let ((json-string (yason:encode-plist message)))
    (write-line json-string (transport-output transport))
    (force-output (transport-output transport))))

(defmethod receive-message ((transport stdio-transport))
  "Receive a JSON-RPC message via stdin"
  (let ((line (read-line (transport-input transport) nil nil)))
    (when line
      (yason:parse line :object-as :plist))))
```

#### 1.2 MCP Message Types
**File**: `src/messages.lisp`

Define MCP-specific message classes:
- Initialize request/response
- Tool list/call messages
- Resource list/read messages
- Prompt list/get messages
- Capability negotiation
- Error handling

### Phase 2: MCP Server Framework (Week 2)

#### 2.1 MCP Server Class
**File**: `src/mcp-server.lisp`

```common-lisp
(openrpc-server:define-api mcp-server
  :title "MCP Server"
  :version "1.0.0")

(defclass mcp-server-implementation ()
  ((capabilities :initarg :capabilities :reader server-capabilities)
   (tools :initform (make-hash-table :test 'equal) :reader server-tools)
   (resources :initform (make-hash-table :test 'equal) :reader server-resources)
   (prompts :initform (make-hash-table :test 'equal) :reader server-prompts)
   (transport :initarg :transport :accessor server-transport)))
```

#### 2.2 MCP Method Handlers
**File**: `src/mcp-methods.lisp`

Implement core MCP methods:
- `initialize` - Protocol handshake
- `tools/list` - List available tools
- `tools/call` - Execute tool
- `resources/list` - List available resources
- `resources/read` - Read resource content
- `prompts/list` - List available prompts
- `prompts/get` - Get prompt template

### Phase 3: MCP Features (Week 3)

#### 3.1 Tools System
**File**: `src/tools.lisp`

```common-lisp
(defmacro define-mcp-tool (name params &body body)
  `(progn
     (register-tool ',name ,params)
     (openrpc-server:define-rpc-method ,(intern (format nil "CALL-TOOL-~A" name)) 
         (arguments)
       (:param arguments object "Tool arguments")
       (:result tool-result)
       ,@body)))

;; Example tool definition
(define-mcp-tool get-weather ((location string))
  "Get current weather for a location"
  (make-instance 'tool-result
                 :content (list (make-text-content 
                               (format nil "Weather in ~A: sunny, 72°F" location)))
                 :is-error nil))
```

#### 3.2 Resources System
**File**: `src/resources.lisp`

Features:
- URI-based resource handling
- Template support (e.g., `file://path/{param}`)
- MIME type handling
- Pagination support
- Binary and text content

#### 3.3 Prompts System
**File**: `src/prompts.lisp`

Features:
- Prompt templates with parameter substitution
- Multi-message conversation support
- Argument validation
- Template rendering

### Phase 4: High-Level API (Week 4)

#### 4.1 MCP DSL
**File**: `src/dsl.lisp`

```common-lisp
(define-mcp-server weather-server
  (:description "Weather information server")
  (:version "1.0.0")
  
  (tool get-weather ((location string))
    "Get weather for a location"
    (fetch-weather-data location))
  
  (resource weather-data "weather://data/{location}"
    :mime-type "application/json"
    (lambda (location) (get-cached-weather location)))
  
  (prompt weather-prompt ((location string) (format string))
    "Generate weather report"
    (make-weather-prompt location format)))
```

#### 4.2 Server Runner
**File**: `src/stdio-runner.lisp`

```common-lisp
(defun run-mcp-stdio-server (&optional (api-class 'default-mcp-api))
  "Entry point for STDIO MCP server"
  (let* ((transport (make-instance 'stdio-transport))
         (api (make-instance api-class))
         (capabilities (list :|tools| (list :|listChanged| t)
                            :|resources| (list :|listChanged| t)
                            :|prompts| (list :|listChanged| t)))
         (server (make-instance 'mcp-server-implementation
                               :transport transport
                               :api api
                               :capabilities capabilities)))
    
    ;; Ensure stdout is line-buffered for proper message delivery
    (setf (stream-line-column *standard-output*) 0)
    
    ;; Start the server loop
    (start-stdio-server server)))
```

### Phase 5: Examples and Testing (Week 5)

#### 5.1 Example Servers
**Directory**: `examples/`

- **Weather Server**: HTTP API integration example
- **File System Server**: Local file access example
- **Calculator Server**: Simple computation tools
- **Database Server**: SQL query interface

#### 5.2 Testing Framework
**Directory**: `t/`

- Unit tests for all components
- Integration tests with mock clients
- Protocol compliance tests
- STDIO transport tests

## STDIO Transport Details

### Message Format
- **Line-delimited JSON**: Each message on a single line
- **No pretty-printing**: Compact JSON format
- **UTF-8 encoding**: Standard text encoding

### Communication Flow
```
Client → Server: {"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}
Server → Client: {"jsonrpc":"2.0","id":1,"result":{...}}
```

### Error Handling
- Use stderr for debug logs, not stdout
- Proper JSON-RPC error responses
- Graceful handling of client disconnection

### Process Management
- Exit cleanly when stdin closes
- Handle signals appropriately
- No background threads preventing exit

## Key Advantages of Using OpenRPC

1. **Mature JSON-RPC**: Battle-tested JSON-RPC implementation
2. **Automatic Serialization**: CLOS objects automatically serialized to JSON
3. **Type System**: Rich type system with JSON Schema generation
4. **Documentation**: Automatic API documentation generation
5. **Client Generation**: Can generate MCP clients automatically
6. **Middleware Support**: Built-in CORS, logging, and other middleware

## Claude Desktop Integration

Configuration example for `claude_desktop_config.json`:
```json
{
  "mcpServers": {
    "weather": {
      "command": "sbcl",
      "args": ["--script", "/path/to/weather-server.lisp"],
      "env": {
        "API_KEY": "your-api-key"
      }
    }
  }
}
```

## File Structure

```
src/
├── core.lisp              # Existing core functionality
├── stdio-transport.lisp   # STDIO transport implementation
├── messages.lisp          # MCP message type definitions
├── mcp-server.lisp        # Main server class and logic
├── mcp-methods.lisp       # MCP method handlers
├── tools.lisp             # Tools system
├── resources.lisp         # Resources system
├── prompts.lisp           # Prompts system
├── dsl.lisp               # High-level DSL
└── stdio-runner.lisp      # Server runner and main entry

examples/
├── weather-server.lisp    # Weather API example
├── file-server.lisp       # File system example
├── calculator.lisp        # Math tools example
└── database-server.lisp   # Database query example

t/
├── stdio-transport-test.lisp
├── mcp-server-test.lisp
├── tools-test.lisp
├── resources-test.lisp
└── integration-test.lisp
```

## Timeline

- **Week 1**: STDIO transport and message handling
- **Week 2**: Core MCP server implementation
- **Week 3**: Tools, resources, and prompts systems
- **Week 4**: High-level API and DSL
- **Week 5**: Examples, testing, and documentation

## Success Criteria

1. **Protocol Compliance**: Full MCP specification compliance
2. **Claude Desktop Integration**: Working with Claude Desktop
3. **Example Servers**: Multiple working example servers
4. **Documentation**: Complete API documentation
5. **Testing**: Comprehensive test suite
6. **Performance**: Efficient message handling and low latency

## Next Steps

1. Set up updated dependencies in `40ants-mcp.asd`
2. Implement basic STDIO transport layer
3. Create MCP message type definitions
4. Build core server framework on OpenRPC foundation
5. Implement tools system with examples
6. Add resources and prompts support
7. Create high-level DSL for easy server definition
8. Build comprehensive examples and tests 