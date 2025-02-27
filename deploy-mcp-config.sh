#!/bin/bash
# deploy-mcp-config.sh - Deploy MCP configuration to Claude Desktop with environment variable expansion
# Usage: ./deploy-mcp-config.sh [source-config.json]

set -e

# Source and destination paths
SOURCE_CONFIG=${1:-"claude_desktop_config.json"}
DEST_DIR="$HOME/Library/Application Support/Claude"
DEST_CONFIG="$DEST_DIR/claude_desktop_config.json"

# Check if source config exists
if [ ! -f "$SOURCE_CONFIG" ]; then
    echo "Error: Source config file '$SOURCE_CONFIG' not found."
    echo "Usage: ./deploy-mcp-config.sh [source-config.json]"
    exit 1
fi

# Check if jq is installed
if ! command -v jq &> /dev/null; then
    echo "Error: jq is not installed. Please install it first."
    echo "Brew: brew install jq"
    echo "Ubuntu/Debian: sudo apt-get install jq"
    exit 1
fi

# Create destination directory if it doesn't exist
mkdir -p "$DEST_DIR"

echo "Deploying MCP configuration:"
echo "  Source: $SOURCE_CONFIG"
echo "  Destination: $DEST_CONFIG"

# Process filesystem paths - replace $HOME and $PWD with actual paths
HOME_PATH=$(echo $HOME | sed 's/\//\\\//g')  # Escape slashes for sed
PWD_PATH=$(echo $PWD | sed 's/\//\\\//g')    # Escape slashes for sed

# Replace both HOME and PWD variables
cat "$SOURCE_CONFIG" | sed "s/\$HOME/$HOME_PATH/g" | sed "s/\$PWD/$PWD_PATH/g" > /tmp/mcp_config_temp.json

# Process environment variables in env section
# This is more complex and requires jq processing for each server's env section
for server in $(jq -r '.mcpServers | keys[]' /tmp/mcp_config_temp.json); do
    # Check if this server has env section
    if jq -e ".mcpServers[\"$server\"].env" /tmp/mcp_config_temp.json > /dev/null; then
        echo "Processing environment variables for $server server..."
        
        # For each environment variable in this server's config
        for env_var in $(jq -r ".mcpServers[\"$server\"].env | keys[]" /tmp/mcp_config_temp.json); do
            env_value=$(jq -r ".mcpServers[\"$server\"].env[\"$env_var\"]" /tmp/mcp_config_temp.json)
            
            # If the value starts with $, try to expand it
            if [[ "$env_value" == \$* ]]; then
                var_name=${env_value:1}  # Remove the $ prefix
                echo "  Checking for environment variable: $var_name"
                
                if [[ -n "${!var_name}" ]]; then
                    expanded_value="${!var_name}"
                    # Show first few characters for debugging
                    masked_value="${expanded_value:0:8}..."
                    echo "  Expanding $env_value to value: $masked_value"
                    
                    # Update the value in the JSON - make sure to escape special characters
                    escaped_value=$(echo "$expanded_value" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g')
                    jq ".mcpServers[\"$server\"].env[\"$env_var\"] = \"$escaped_value\"" /tmp/mcp_config_temp.json > /tmp/mcp_config_temp2.json
                    mv /tmp/mcp_config_temp2.json /tmp/mcp_config_temp.json
                else
                    echo "  Warning: Environment variable $var_name not found in environment"
                    echo "  Current environment variables: $(env | grep -v PATH | grep -v SECRET | grep -v TOKEN | cut -d= -f1 | sort)"
                fi
            fi
        done
    fi
done

# Copy the processed config to destination
cp /tmp/mcp_config_temp.json "$DEST_CONFIG"
rm /tmp/mcp_config_temp.json

echo "Configuration deployed successfully!"
echo "You may need to restart Claude Desktop for changes to take effect."
