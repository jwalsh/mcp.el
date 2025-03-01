#!/bin/bash

# setup-servers.sh - Install and configure MCP servers for local development
set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${GREEN}Setting up MCP servers for use with Claude and Emacs...${NC}"

# Update package.json to include MCP server dependencies
echo -e "${YELLOW}Adding MCP server dependencies to package.json...${NC}"

# Check if jq is installed
if ! command -v jq &> /dev/null; then
    echo -e "${RED}Error: jq is not installed. Please install it first.${NC}"
    echo "Brew: brew install jq"
    echo "Ubuntu/Debian: sudo apt-get install jq"
    exit 1
fi

# Add MCP server dependencies to package.json
jq '.dependencies = {
    "@modelcontextprotocol/server-filesystem": "latest",
    "@modelcontextprotocol/server-github": "latest",
    "@modelcontextprotocol/server-time": "latest",
    "@modelcontextprotocol/server-memory": "latest",
    "@modelcontextprotocol/server-fetch": "latest",
    "@modelcontextprotocol/server-sqlite": "latest"
} | .scripts.start = "node run-servers.js"' package.json > package.json.tmp && mv package.json.tmp package.json

echo -e "${GREEN}Updated package.json with MCP server dependencies${NC}"

# Update pyproject.toml to include Python-based MCP server dependencies
echo -e "${YELLOW}Adding Python-based MCP server dependencies to pyproject.toml...${NC}"

# Using a more macOS-compatible approach
if grep -q "\[tool.poetry.dependencies\]" pyproject.toml; then
    # Create a temporary file with the updated content
    awk '
    /\[tool.poetry.dependencies\]/ {
        print $0;
        print "mcp-server-git = \"^0.1.0\"";
        print "uvx = \"^0.1.0\"";
        next;
    }
    { print $0 }
    ' pyproject.toml > pyproject.toml.new
    
    # Replace the original file
    mv pyproject.toml.new pyproject.toml
    echo -e "${GREEN}Updated pyproject.toml with Python MCP server dependencies${NC}"
else
    echo -e "${RED}Could not find [tool.poetry.dependencies] section in pyproject.toml${NC}"
    echo -e "${YELLOW}Please manually add the following dependencies:${NC}"
    echo "mcp-server-git = \"^0.1.0\""
    echo "uvx = \"^0.1.0\""
fi

# Install dependencies
echo -e "${YELLOW}Installing Node.js dependencies...${NC}"
npm install

echo -e "${YELLOW}Installing Python dependencies...${NC}"
poetry install

# Create a Node.js script to run all servers
echo -e "${YELLOW}Creating a script to run all MCP servers...${NC}"

cat > run-servers.js << 'EOF'
const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

// Read the Claude config file to get server configurations
const configPath = path.resolve(process.env.CONFIG_FILE || './claude_desktop_config.json');
let config;

try {
  config = JSON.parse(fs.readFileSync(configPath, 'utf8'));
} catch (error) {
  console.error(`Error reading config file at ${configPath}:`, error.message);
  process.exit(1);
}

if (!config.mcpServers) {
  console.error('Invalid config file: missing mcpServers section');
  process.exit(1);
}

// Function to start a server process
function startServer(name, serverConfig) {
  console.log(`Starting ${name} server...`);
  
  const { command, args, env } = serverConfig;
  
  // Combine process.env with the server's env config
  const serverEnv = { ...process.env };
  if (env) {
    Object.entries(env).forEach(([key, value]) => {
      // Handle environment variable references (e.g. $GITHUB_TOKEN)
      if (typeof value === 'string' && value.startsWith('$')) {
        const envVarName = value.substring(1);
        serverEnv[key] = process.env[envVarName] || value;
      } else {
        serverEnv[key] = value;
      }
    });
  }
  
  // Spawn the process
  const serverProcess = spawn(command, args, { 
    env: serverEnv,
    stdio: 'pipe' // Capture output
  });
  
  // Setup logging with server name prefix
  serverProcess.stdout.on('data', (data) => {
    console.log(`[${name}] ${data.toString().trim()}`);
  });
  
  serverProcess.stderr.on('data', (data) => {
    console.error(`[${name}] ERROR: ${data.toString().trim()}`);
  });
  
  serverProcess.on('close', (code) => {
    console.log(`[${name}] Server process exited with code ${code}`);
    // Restart the server after a delay
    setTimeout(() => {
      console.log(`[${name}] Restarting server...`);
      startServer(name, serverConfig);
    }, 5000);
  });
  
  return serverProcess;
}

// Start all servers from the config
console.log('Starting MCP servers...');
const serverProcesses = {};

Object.entries(config.mcpServers).forEach(([name, serverConfig]) => {
  serverProcesses[name] = startServer(name, serverConfig);
});

// Handle graceful shutdown
process.on('SIGINT', () => {
  console.log('\nGracefully shutting down servers...');
  Object.entries(serverProcesses).forEach(([name, process]) => {
    console.log(`Stopping ${name} server...`);
    process.kill();
  });
  
  // Give processes a moment to shut down cleanly
  setTimeout(() => {
    console.log('All servers stopped. Exiting.');
    process.exit(0);
  }, 1000);
});

console.log('All MCP servers started. Press Ctrl+C to stop.');
EOF

# Make run-servers.js executable
chmod +x run-servers.js

echo -e "${GREEN}Created run-servers.js script${NC}"

# Create or update run-servers.sh
cat > run-servers.sh << 'EOF'
#!/bin/bash
# Start all MCP servers defined in claude_desktop_config.json

CONFIG_FILE="${1:-claude_desktop_config.json}"

if [ ! -f "$CONFIG_FILE" ]; then
    echo "Error: Config file '$CONFIG_FILE' not found."
    echo "Usage: ./run-servers.sh [path/to/config.json]"
    exit 1
fi

echo "Starting MCP servers using config from: $CONFIG_FILE"
CONFIG_FILE="$CONFIG_FILE" node run-servers.js
EOF

# Make run-servers.sh executable
chmod +x run-servers.sh

echo -e "${GREEN}MCP server setup complete!${NC}"
echo -e "${YELLOW}To start all MCP servers, run:${NC}"
echo -e "  ./run-servers.sh [path/to/config.json]"
echo -e "${YELLOW}Make sure your environment variables (like GITHUB_TOKEN) are set before running.${NC}"
