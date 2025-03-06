# MCP Servers for Emacs

This package provides a way to use Model Context Protocol (MCP) servers with Emacs, similar to how Claude Desktop works. It allows you to start, manage, and interact with MCP servers like filesystem, git, memory, and more.

## Quick Setup

1. Make sure you have:
   - `npx` (Node.js)
   - `uvx` (Python package for running uvicorn-based servers)
   - Various MCP server packages (see "Installing MCP Servers" below)

2. Check your configuration:
   ```bash
   # Download the validation script
   curl -L https://gist.githubusercontent.com/jwalsh/e114c882bade5d662a9a7eac8fa5b109/raw/sjg-evaluate-claude-desktop.sh -o sjg-evaluate-claude-desktop.sh
   chmod +x sjg-evaluate-claude-desktop.sh
   
   # Run the validation script
   ./sjg-evaluate-claude-desktop.sh
   ```

3. In your Emacs configuration:
   ```elisp
   ;; Load the mcp-servers package
   (require 'mcp-servers)
   
   ;; Optionally, auto-start MCP servers when Emacs starts
   (setq mcp-servers-auto-start t)
   
   ;; Enable the mcp-servers mode
   (mcp-servers-mode 1)
   ```

## Installing MCP Servers

You need to install the required MCP server packages:

```bash
# Node.js packages
npm install -g @modelcontextprotocol/server-filesystem
npm install -g @modelcontextprotocol/server-memory
npm install -g @modelcontextprotocol/server-time
npm install -g @jwalsh/mcp-server-qrcode

# Python packages 
pip install mcp-server-git
pip install uvx
```

## Configuration

The package will look for a `claude_desktop_config.json` file in the following locations:
- Current directory
- `dist/claude_desktop_config.json` in current directory
- `~/.config/Claude/claude_desktop_config.json`
- `~/Library/Application Support/Claude/claude_desktop_config.json`

You can also specify a custom location:
```elisp
(setq mcp-servers-config-file "/path/to/your/claude_desktop_config.json")
```

The configuration file format is the same as Claude Desktop:

```json
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
    }
  }
}
```


## Usage

### Managing Servers

- `M-x mcp-servers-start-all` - Start all configured MCP servers
- `M-x mcp-servers-stop-all` - Stop all running MCP servers
- `M-x mcp-servers-restart-all` - Restart all MCP servers
- `M-x mcp-servers-status` - Show the status of all MCP servers

With default keybindings (under `C-c m` prefix):
- `C-c m s` - Start all servers
- `C-c m S` - Stop all servers
- `C-c m r` - Restart all servers
- `C-c m ?` - Show server status
- `C-c m t` - List all available tools
- `C-c m c` - Call a tool interactively

### Working with Tools

```elisp
;; Get a connection to a server
(let ((conn (mcp-servers-get-connection "filesystem")))
  ;; Call a tool
  (mcp-async-call-tool 
   conn "list-files" 
   '(:path "/path/to/dir" :maxDepth 1)
   (lambda (result)
     (message "Got files: %s" (gethash "items" result)))
   (lambda (code message)
     (message "Error: %s - %s" code message))))
```

## Example Functions

The package includes example functions demonstrating how to use different MCP servers:

- `generate-qrcode` - Create a QR code from a URL
- `browse-mcp-fs` - Browse the filesystem using the MCP filesystem server
- `git-status-mcp` - Show git status for a repository

## Extending

To add support for additional MCP servers, simply include them in your configuration file and restart the servers. The package will automatically detect and connect to all configured servers.

## Troubleshooting

If you encounter issues with MCP servers:

1. Check the server buffers (`*mcp-server-NAME*`) for error messages
2. Ensure all required packages are installed
3. Try running the standalone validation script (`sjg-evaluate-claude-desktop.sh`)
4. Try starting each server manually to check for errors

Remember that each server requires specific Node.js or Python packages to be installed.
