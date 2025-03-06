#!/usr/bin/env bash
# validate-config.sh - Script to validate MCP server configurations
# Usage: ./validate-config.sh path/to/config.json

set -e

CONFIG_FILE=${1:-"claude_desktop_config.json"}

if [ ! -f "$CONFIG_FILE" ]; then
    echo "Error: Config file '$CONFIG_FILE' not found."
    echo "Usage: ./validate-config.sh path/to/config.json"
    exit 1
fi

# Check if jq is installed
if ! command -v jq &> /dev/null; then
    echo "Error: jq is not installed. Please install it first."
    echo "Brew: brew install jq"
    echo "Ubuntu/Debian: sudo apt-get install jq"
    exit 1
fi

# Extract server names
SERVER_NAMES=$(jq -r '.mcpServers | keys[]' "$CONFIG_FILE")

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
    if timeout 2 env $env_vars $command $args > /tmp/mcp_output 2>&1; then
        echo "✅ Server command executed successfully (timeout applied):"
        head -n 5 /tmp/mcp_output
    else
        # Check if the exit code is 124, which means the timeout was reached
        # This is actually expected as servers run continuously
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "✅ Server started and running (stopped by timeout as expected):"
            head -n 5 /tmp/mcp_output
            
            # Try to get tools list
            get_server_tools "$server_name"
        else
            echo "❌ Error: Server command failed with exit code $exit_code:"
            cat /tmp/mcp_output
            return 1
        fi
    fi
    
    echo "---------------------------------------------"
    return 0
}

# Priority order for servers (most useful and no secrets first)
PRIORITY_SERVERS=("filesystem" "time" "memory" "git" "fetch" "sqlite")

# First validate priority servers
for server in "${PRIORITY_SERVERS[@]}"; do
    if echo "$SERVER_NAMES" | grep -q "^$server$"; then
        validate_server "$server"
    fi
done

# Then validate remaining servers
for server in $SERVER_NAMES; do
    if ! echo "${PRIORITY_SERVERS[@]}" | grep -q "$server"; then
        validate_server "$server"
    fi
done

# Check for validation success
if [ $? -eq 0 ]; then
    echo "Validation complete. All servers appear to be configured correctly."
    
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
            echo "Found Claude Desktop configuration directory: $dir"
            break
        fi
    done
    
    # Ask about deployment
    if [ -n "$FOUND_LOCATION" ]; then
        read -p "Would you like to deploy this configuration to Claude Desktop? (y/n): " DEPLOY
        if [[ "$DEPLOY" =~ ^[Yy]$ ]]; then
            # Create backup if existing config exists
            if [ -f "$FOUND_LOCATION/claude_desktop_config.json" ]; then
                cp "$FOUND_LOCATION/claude_desktop_config.json" "$FOUND_LOCATION/claude_desktop_config.backup.$(date +%Y%m%d%H%M%S).json"
                echo "Created backup of existing configuration"
            fi
            
            # Copy the config
            mkdir -p "$FOUND_LOCATION"
            cp "$CONFIG_FILE" "$FOUND_LOCATION/claude_desktop_config.json"
            echo "Configuration deployed successfully! Restart Claude Desktop for changes to take effect."
        else
            echo "Configuration not deployed. You can manually deploy it later with:"
            echo "  cp $CONFIG_FILE \"$FOUND_LOCATION/claude_desktop_config.json\""
        fi
    else
        echo "No Claude Desktop installation was found."
        echo "You may be using FreeBSD or another platform where Claude Desktop is not installed."
        echo "For manual deployment, copy the config to your Claude Desktop configuration location when available."
    fi
else
    echo "Validation failed. Please fix the issues before deploying."
fi
