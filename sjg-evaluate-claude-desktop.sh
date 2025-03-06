#!/usr/bin/env bash
# sjg-evaluate-claude-desktop.sh - A standalone script to test and deploy Claude Desktop MCP configuration
# Created by Claude on behalf of jwalsh
# Usage: ./sjg-evaluate-claude-desktop.sh

set -e

# Color output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

echo -e "${BLUE}${BOLD}Claude Desktop MCP Server Evaluation Script${NC}"
echo -e "${BLUE}=======================================${NC}"
echo -e "This script will:"
echo -e "  1. Check for required dependencies"
echo -e "  2. Generate an MCP server configuration file"
echo -e "  3. Validate the MCP servers"
echo -e "  4. Optionally deploy the configuration to Claude Desktop"
echo -e ""

# =====================================================================
# Check for required dependencies
# =====================================================================
echo -e "${BOLD}Checking for required dependencies...${NC}"

DEPS=("jq" "npx" "uvx")
MISSING=()

for dep in "${DEPS[@]}"; do
  if ! command -v "$dep" &>/dev/null; then
    MISSING+=("$dep")
  fi
done

if [ ${#MISSING[@]} -ne 0 ]; then
  echo -e "${RED}Missing required dependencies: ${MISSING[*]}${NC}"
  
  # Provide installation instructions
  echo -e "${YELLOW}Installation instructions:${NC}"
  for missing in "${MISSING[@]}"; do
    case $missing in
      jq)
        echo "  jq: Run 'brew install jq' (macOS) or 'apt-get install jq' (Debian/Ubuntu)"
        ;;
      npx)
        echo "  npx: Run 'npm install -g npx' (requires Node.js)"
        ;;
      uvx)
        echo "  uvx: Run 'pip install uvx' (requires Python)"
        ;;
    esac
  done
  exit 1
fi

echo -e "${GREEN}All dependencies are installed.${NC}"

# =====================================================================
# Create a temporary directory for files
# =====================================================================
TEMP_DIR=$(mktemp -d)
CONFIG_FILE="$TEMP_DIR/claude_desktop_config.json"

# Ensure temporary directory is cleaned up on exit
trap 'rm -rf "$TEMP_DIR"' EXIT

# =====================================================================
# Inline MCP server configuration template
# =====================================================================
echo -e "${BOLD}Generating MCP server configuration...${NC}"

cat > "$CONFIG_FILE" << 'EOF'
{
  "mcpServers": {
    "filesystem": {
      "args": [
        "-y",
        "@modelcontextprotocol/server-filesystem",
        "$HOME/projects",
        "$HOME/Desktop"
      ],
      "command": "npx"
    },
    "git": {
      "args": [
        "mcp-server-git"
      ],
      "command": "uvx"
    },
    "memory": {
      "args": [
        "-y",
        "@modelcontextprotocol/server-memory"
      ],
      "command": "npx"
    },
    "puppeteer": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-puppeteer"]
    },
    "qrcode": {
      "args": [
        "-y",
        "@jwalsh/mcp-server-qrcode"
      ],
      "command": "npx"
    },
    "sequential-thinking": {
      "args": [
        "-y",
        "@modelcontextprotocol/server-sequential-thinking"
      ],
      "command": "npx"
    }
  }
}
EOF

# =====================================================================
# Process the template, replacing environment variables
# =====================================================================
HOME_PATH=$(echo $HOME | sed 's/\//\\\//g')  # Escape slashes for sed
TMP_CONFIG="$TEMP_DIR/tmp_config.json"

# Replace environment variables in the configuration
sed "s/\$HOME/$HOME_PATH/g" "$CONFIG_FILE" > "$TMP_CONFIG"
mv "$TMP_CONFIG" "$CONFIG_FILE"

echo -e "${GREEN}Configuration generated at: $CONFIG_FILE${NC}"

# =====================================================================
# Define validation functions
# =====================================================================

# Function to query server for tools using direct pipe
get_server_tools() {
    local server_name=$1
    local command=$(jq -r ".mcpServers.\"$server_name\".command" "$CONFIG_FILE")
    local args=$(jq -r ".mcpServers.\"$server_name\".args | map(.) | join(\" \")" "$CONFIG_FILE")
    
    echo "Listing available tools for $server_name..."
    # Create JSON-RPC request for tools/list
    echo '{"method":"tools/list","params":{},"id":1,"jsonrpc":"2.0"}' | $command $args 2>/dev/null | jq -r '.result.tools[] | "  - \(.name): \(.description // "(No description)")"' || {
        echo "  ⚠️ Could not list tools from server"
        return 1
    }
}

# Function to validate a server
validate_server() {
    local server_name=$1
    local command=$(jq -r ".mcpServers.\"$server_name\".command" "$CONFIG_FILE")
    local args=$(jq -r ".mcpServers.\"$server_name\".args | map(.) | join(\" \")" "$CONFIG_FILE")
    local env_vars=$(jq -r ".mcpServers.\"$server_name\".env // {} | to_entries | map(\"\(.key)='\(.value)'\") | join(\" \")" "$CONFIG_FILE")
    
    echo "---------------------------------------------"
    echo "Testing server: $server_name"
    echo "Command: $command $args"
    
    # Check if command exists
    if ! command -v $command &> /dev/null; then
        echo "❌ Error: Command '$command' not found. Please install it first."
        return 1
    fi
    
    # Replace environment variable placeholders with actual values if they exist
    if [[ "$env_vars" == *"<YOUR_"* ]]; then
        echo "⚠️  Warning: Environment variables contain placeholders:"
        echo "   $env_vars"
        echo "   Attempting to use system environment variables instead."
        
        # Try to extract and use actual environment variables if they exist
        for var in $(echo "$env_vars" | grep -o '<YOUR_[A-Z_]*>' | sed 's/<YOUR_\(.*\)>/\1/g'); do
            if [[ -n "${!var}" ]]; then
                echo "   Found $var in environment, will use it"
                env_vars=$(echo "$env_vars" | sed "s/<YOUR_$var>/${!var}/g")
            else
                echo "❌ Error: Environment variable $var not found in system environment."
                echo "   Please set it with: export $var=your_value"
                return 1
            fi
        done
    fi
    
    # Run the command with a timeout to avoid hanging
    # MCP servers don't typically use --help, so we'll just run the command for a brief moment
    echo "Running: env $env_vars $command $args 2>&1 | head -n 5"
    if timeout 2 env $env_vars $command $args > "$TEMP_DIR/mcp_output" 2>&1; then
        echo "✅ Server command executed successfully (timeout applied):"
        head -n 5 "$TEMP_DIR/mcp_output"
        
        # Try to list tools if command executed successfully
        get_server_tools "$server_name"
    else
        # Check if the exit code is 124, which means the timeout was reached
        # This is actually expected as servers run continuously
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "✅ Server started and running (stopped by timeout as expected):"
            head -n 5 "$TEMP_DIR/mcp_output"
            
            # Try to list tools if server is running
            get_server_tools "$server_name"
        else
            echo "❌ Error: Server command failed with exit code $exit_code:"
            cat "$TEMP_DIR/mcp_output"
            return 1
        fi
    fi
    
    echo "---------------------------------------------"
    return 0
}

# =====================================================================
# Validate servers
# =====================================================================
echo -e "${BOLD}Validating MCP servers...${NC}"

# Extract server names
SERVER_NAMES=$(jq -r '.mcpServers | keys[]' "$CONFIG_FILE")

# Priority order for servers (most useful and no secrets first)
PRIORITY_SERVERS=("filesystem" "time" "memory" "git" "fetch" "sqlite")

# Track validation success
VALIDATION_SUCCESS=true

# First validate priority servers
for server in "${PRIORITY_SERVERS[@]}"; do
    if echo "$SERVER_NAMES" | grep -q "^$server$"; then
        validate_server "$server" || VALIDATION_SUCCESS=false
    fi
done

# Then validate remaining servers
for server in $SERVER_NAMES; do
    if ! echo "${PRIORITY_SERVERS[@]}" | grep -q "$server"; then
        validate_server "$server" || VALIDATION_SUCCESS=false
    fi
done

# =====================================================================
# Check for Claude Desktop installation and offer deployment
# =====================================================================
if $VALIDATION_SUCCESS; then
    echo -e "${GREEN}Validation complete. All servers appear to be configured correctly.${NC}"
    
    # Check possible Claude Desktop installation locations
    CLAUDE_DIRS=(
        "$HOME/Library/Application Support/Claude"               # Standard macOS location
        "$HOME/Library/Containers/com.anthropic.claude/Data/Library/Application Support/Claude"  # Alternative macOS location
        "$HOME/.config/Claude"                                  # Linux location
        "/Applications/Claude.app/Contents/Resources/config"     # Another possible macOS location
    )
    
    FOUND_LOCATION=""
    for dir in "${CLAUDE_DIRS[@]}"; do
        if [ -d "$dir" ]; then
            FOUND_LOCATION="$dir"
            echo -e "${GREEN}Found Claude Desktop configuration directory: $dir${NC}"
            break
        fi
    done
    
    # Ask about deployment
    if [ -n "$FOUND_LOCATION" ]; then
        read -p "Would you like to deploy this configuration to Claude Desktop? (y/n): " DEPLOY
        if [[ "$DEPLOY" =~ ^[Yy]$ ]]; then
            # Create backup if existing config exists
            if [ -f "$FOUND_LOCATION/claude_desktop_config.json" ]; then
                BACKUP_FILE="$FOUND_LOCATION/claude_desktop_config.backup.$(date +%Y%m%d%H%M%S).json"
                cp "$FOUND_LOCATION/claude_desktop_config.json" "$BACKUP_FILE"
                echo -e "${GREEN}Created backup of existing configuration at $BACKUP_FILE${NC}"
            fi
            
            # Copy the config
            mkdir -p "$FOUND_LOCATION"
            cp "$CONFIG_FILE" "$FOUND_LOCATION/claude_desktop_config.json"
            echo -e "${GREEN}Configuration deployed successfully!${NC}"
            echo -e "${YELLOW}IMPORTANT: Restart Claude Desktop for changes to take effect.${NC}"
        else
            echo -e "${YELLOW}Configuration not deployed. You can manually deploy it later with:${NC}"
            echo -e "  cp $CONFIG_FILE \"$FOUND_LOCATION/claude_desktop_config.json\""
        fi
    else
        echo -e "${YELLOW}No Claude Desktop installation was found.${NC}"
        echo -e "You may be using FreeBSD or another platform where Claude Desktop is not installed."
        echo -e "For manual deployment, copy the config to your Claude Desktop configuration location when available."
        
        # Offer to save the configuration file
        read -p "Would you like to save the configuration file to the current directory? (y/n): " SAVE
        if [[ "$SAVE" =~ ^[Yy]$ ]]; then
            cp "$CONFIG_FILE" "./claude_desktop_config.json"
            echo -e "${GREEN}Configuration saved to ./claude_desktop_config.json${NC}"
        fi
    fi
else
    echo -e "${RED}Validation failed. Please fix the issues before deploying.${NC}"
    
    # Offer to save the configuration file even if validation failed
    read -p "Would you like to save the configuration file for debugging? (y/n): " SAVE
    if [[ "$SAVE" =~ ^[Yy]$ ]]; then
        cp "$CONFIG_FILE" "./claude_desktop_config.json"
        echo -e "${GREEN}Configuration saved to ./claude_desktop_config.json${NC}"
    fi
fi

echo -e "${BLUE}${BOLD}Evaluation complete!${NC}"