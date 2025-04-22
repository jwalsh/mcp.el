#!/usr/bin/env bash
# Start all MCP servers defined in claude_desktop_config.json
# https://modelcontextprotocol.io/quickstart/user

CONFIG_FILE="${1:-claude_desktop_config.json}"

if [ ! -f "$CONFIG_FILE" ]; then
    echo "Error: Config file '$CONFIG_FILE' not found."
    echo "Usage: ./run-servers.sh [path/to/config.json]"
    exit 1
fi

echo "Starting MCP servers using config from: $CONFIG_FILE"
CONFIG_FILE="$CONFIG_FILE" node run-servers.js
