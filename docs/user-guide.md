# MCP.el User Guide

## Introduction

MCP.el is an Emacs client for interfacing with the Model Context Protocol (MCP), enabling structured communication between Emacs and external processes.

## Getting Started

### Installation

1. Install Emacs 27.1 or later
2. Install MCP.el through MELPA:
   ```elisp
   M-x package-install RET mcp RET
   ```

### Basic Configuration

Add to your Emacs configuration:
```elisp
(require 'mcp)
```

## Basic Usage

### Establishing Connections

```elisp
;; Connect to a filesystem-based MCP server
(mcp-connect-server "filesystem" 
                    "npx" 
                    '("-y" "@modelcontextprotocol/server-filesystem" "~/Downloads/"))
```

### Using Tools

```elisp
;; Create a file writing tool
(mcp-make-text-gptel-tool "filesystem" "write_file")

;; Use the tool synchronously
(let ((connection (gethash "filesystem" mcp-server-connections)))
  (mcp-call-tool "write_file" 
                 '(:path "example.txt" 
                   :content "Hello, World!")))
```

### Handling Prompts

```elisp
;; Get a prompt synchronously
(let ((connection (gethash "server-name" mcp-server-connections)))
  (mcp-get-prompt connection "prompt_name" '(:temperature "1.0")))
```

### Managing Resources

```elisp
;; Read a resource synchronously
(let ((connection (gethash "server-name" mcp-server-connections)))
  (mcp-read-resource connection "resource://path"))
```

## Advanced Usage

### Asynchronous Operations

```elisp
;; Asynchronous tool call
(mcp-async-call-tool connection
                     "tool_name"
                     '(:param "value")
                     #'success-callback
                     #'error-callback)
```

### Error Handling

```elisp
(mcp-async-call-tool connection
                     "tool_name"
                     '(:param "value")
                     #'(lambda (result)
                         (message "Success: %s" result))
                     #'(lambda (code message)
                         (message "Error %s: %s" code message)))
```

## Best Practices

1. Always handle errors in asynchronous operations
2. Use connection callbacks for initialization
3. Clean up connections when they're no longer needed
4. Validate tool parameters before sending

## Troubleshooting

### Common Issues

1. Connection failures
   - Verify server is running
   - Check network connectivity
   - Validate server URL/path

2. Tool execution errors
   - Verify tool exists on server
   - Check parameter types and values
   - Review server logs

### Debug Mode

Enable debug mode for detailed logging:
```elisp
(setq mcp-debug t)
```

## Further Reading

- [Technical Guide](technical-guide.org)
- [API Reference](api-reference.md)
- [MCP Protocol Documentation](https://modelcontextprotocol.io/introduction)