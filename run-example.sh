#!/bin/bash

# Run the simple MCP server example
# 
# This script loads the 40ants-mcp system and runs the example server
# using SBCL. The server will communicate via STDIO as per MCP spec.

# Exit on any error
set -e

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL is required but not found in PATH" >&2
    exit 1
fi

# Run the example server
sbcl --noinform \
     --disable-debugger \
     --eval "(require 'asdf)" \
     --eval "(asdf:load-system '40ants-mcp)" \
     --eval "(asdf:load-system '40ants-mcp/examples/simple-server)" \
     --eval "(40ants-mcp/examples/simple-server:main)" \
     --quit 