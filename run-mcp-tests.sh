#!/bin/bash
# run-mcp-tests.sh - Automated MCP server test runner
# This script tests each MCP server configured in Claude Desktop

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=====================================${NC}"
echo -e "${BLUE}MCP Server Automated Test Runner${NC}"
echo -e "${BLUE}=====================================${NC}\n"

# Function to test if a command exists
command_exists() {
  command -v "$1" &> /dev/null
}

# Test required commands
echo -e "${BLUE}Checking required commands...${NC}"
REQUIRED_COMMANDS=("npx" "uvx" "jq" "curl")
MISSING_COMMANDS=()

for cmd in "${REQUIRED_COMMANDS[@]}"; do
  if command_exists "$cmd"; then
    echo -e "  ${GREEN}✓${NC} $cmd"
  else
    echo -e "  ${RED}✗${NC} $cmd"
    MISSING_COMMANDS+=("$cmd")
  fi
done

if [ ${#MISSING_COMMANDS[@]} -ne 0 ]; then
  echo -e "\n${RED}Error: Missing required commands: ${MISSING_COMMANDS[*]}${NC}"
  echo "Please install the missing commands and try again."
  exit 1
fi

# Function to test a server
test_server() {
  local server_name=$1
  local command=$2
  local args=$3
  local env_vars=$4
  
  echo -e "\n${BLUE}Testing $server_name server...${NC}"
  echo -e "Command: $command $args"
  
  # Run the server with a timeout
  echo -e "Starting server with timeout..."
  if timeout 3 env $env_vars $command $args > /tmp/mcp_test_output 2>&1; then
    echo -e "${GREEN}✓ Server started successfully (stopped by timeout as expected)${NC}"
  else
    # Check if timeout was reached (exit code 124)
    exit_code=$?
    if [ $exit_code -eq 124 ]; then
      echo -e "${GREEN}✓ Server running (stopped by timeout as expected)${NC}"
    else
      echo -e "${RED}✗ Server failed to start (exit code $exit_code)${NC}"
      echo -e "${RED}Output:${NC}"
      cat /tmp/mcp_test_output
      return 1
    fi
  fi
  
  # Show first few lines of output
  echo -e "\nServer output:"
  head -n 5 /tmp/mcp_test_output
  
  echo -e "\n${GREEN}$server_name server test passed${NC}"
  return 0
}

# Get Claude Desktop config
CONFIG_FILE="$HOME/Library/Application Support/Claude/claude_desktop_config.json"

if [ ! -f "$CONFIG_FILE" ]; then
  echo -e "${RED}Error: Claude Desktop config file not found at $CONFIG_FILE${NC}"
  echo "Run 'make config-deploy' to deploy the configuration first."
  exit 1
fi

echo -e "\n${BLUE}Found Claude Desktop config at $CONFIG_FILE${NC}"

# Get list of configured servers
echo -e "\n${BLUE}Configured MCP Servers:${NC}"
SERVER_NAMES=$(jq -r '.mcpServers | keys[]' "$CONFIG_FILE")
for server in $SERVER_NAMES; do
  echo "  - $server"
done

# Test each server
PASSED=0
FAILED=0
SKIPPED=0

echo -e "\n${BLUE}Running server tests...${NC}"
for server in $SERVER_NAMES; do
  # Extract server configuration
  command=$(jq -r ".mcpServers.\"$server\".command" "$CONFIG_FILE")
  args=$(jq -r ".mcpServers.\"$server\".args | map(.) | join(\" \")" "$CONFIG_FILE")
  env_vars=$(jq -r ".mcpServers.\"$server\".env // {} | to_entries | map(\"\(.key)='\(.value)'\") | join(\" \")" "$CONFIG_FILE")
  
  # Skip if command doesn't exist
  if ! command_exists "$command"; then
    echo -e "\n${YELLOW}Skipping $server server - command '$command' not found${NC}"
    SKIPPED=$((SKIPPED+1))
    continue
  fi
  
  # Run the test
  if test_server "$server" "$command" "$args" "$env_vars"; then
    PASSED=$((PASSED+1))
  else
    FAILED=$((FAILED+1))
  fi
done

# Summary
echo -e "\n${BLUE}=====================================${NC}"
echo -e "${BLUE}Test Summary${NC}"
echo -e "${BLUE}=====================================${NC}"
echo -e "Servers tested: $((PASSED+FAILED+SKIPPED))"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"
echo -e "${YELLOW}Skipped: $SKIPPED${NC}"

if [ $FAILED -eq 0 ]; then
  echo -e "\n${GREEN}All tests passed successfully!${NC}"
  echo -e "Claude Desktop is properly configured with MCP servers."
  echo -e "Use mcp-server-test-suite.md for interactive testing with Claude."
  exit 0
else
  echo -e "\n${RED}Some tests failed.${NC}"
  echo -e "Check the output above for details."
  exit 1
fi