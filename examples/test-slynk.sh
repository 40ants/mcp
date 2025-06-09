#!/bin/bash

# Test script to run MCP server with SLYNK debugging enabled
# 
# This script demonstrates how to start the MCP server with SLYNK
# for remote debugging and development.

echo "🚀 Starting MCP Server with SLYNK debugging..."
echo ""
echo "SLYNK will be available on localhost:4005"
echo "Connect your editor to localhost:4005 for interactive development"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

# Set SLYNK environment variables
export SLYNK_PORT=4005
export SLYNK_INTERFACE=localhost

# Run the server
exec ./examples/simple-server.ros 