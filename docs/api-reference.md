# MCP.el API Reference

## Core Functions

### Connection Management

#### `mcp-connect-server`
```elisp
(mcp-connect-server name command args &key initial-callback tools-callback prompts-callback resources-callback)
```
Establishes a connection to an MCP server.

- `name`: String identifier for the connection
- `command`: Command to start the server
- `args`: List of command arguments
- `initial-callback`: Function called after initial connection
- `tools-callback`: Function called when tools are available
- `prompts-callback`: Function called when prompts are available
- `resources-callback`: Function called when resources are available

#### `mcp-stop-server`
```elisp
(mcp-stop-server name)
```
Disconnects from a named server.

### Tool Operations

#### `mcp-make-text-gptel-tool`
```elisp
(mcp-make-text-gptel-tool server-name tool-name)
```
Creates a gptel-compatible tool.

#### `mcp-call-tool`
```elisp
(mcp-call-tool tool-name params)
```
Synchronously calls a tool.

#### `mcp-async-call-tool`
```elisp
(mcp-async-call-tool connection tool-name params success-callback error-callback)
```
Asynchronously calls a tool.

### Prompt Operations

#### `mcp-get-prompt`
```elisp
(mcp-get-prompt connection prompt-name params)
```
Synchronously retrieves a prompt.

#### `mcp-async-get-prompt`
```elisp
(mcp-async-get-prompt connection prompt-name params success-callback error-callback)
```
Asynchronously retrieves a prompt.

### Resource Operations

#### `mcp-read-resource`
```elisp
(mcp-read-resource connection resource-uri)
```
Synchronously reads a resource.

#### `mcp-async-read-resource`
```elisp
(mcp-async-read-resource connection resource-uri callback)
```
Asynchronously reads a resource.

#### `mcp-async-list-resource-templates`
```elisp
(mcp-async-list-resource-templates connection callback)
```
Asynchronously lists available resource templates.

## Variables

### `mcp-server-connections`
Hash table storing active server connections.

### `mcp-debug`
Boolean controlling debug output.

## Types

### Connection Object
```elisp
(:name string
 :process process
 :tools (hash-table)
 :prompts (hash-table)
 :resources (hash-table))
```

### Tool Definition
```elisp
(:function function
 :name string
 :async boolean
 :description string
 :args (list of parameters)
 :category string)
```

### Resource Template
```elisp
(:uri string
 :parameters (list of parameters))
```

## Error Handling

### Error Callbacks
Error callbacks receive two parameters:
- `code`: Error code string
- `message`: Error description string

Example error callback:
```elisp
(lambda (code message)
  (message "Error %s: %s" code message))
```

## Customization

### Variables
```elisp
(defcustom mcp-default-timeout 30
  "Default timeout in seconds for synchronous operations.")

(defcustom mcp-max-message-size 1048576
  "Maximum size in bytes for messages.")
```

## Events

### Connection Events
- `:initial-connection`
- `:tools-available`
- `:prompts-available`
- `:resources-available`
- `:connection-closed`

## Example Usage

```elisp
;; Initialize connection with all callbacks
(mcp-connect-server 
 "example"
 "npx"
 '("-y" "@modelcontextprotocol/server-example")
 :initial-callback (lambda (conn) 
                    (message "Connected: %s" (jsonrpc-name conn)))
 :tools-callback (lambda (conn tools) 
                  (message "Tools: %s" tools))
 :prompts-callback (lambda (conn prompts) 
                    (message "Prompts: %s" prompts))
 :resources-callback (lambda (conn resources) 
                      (message "Resources: %s" resources)))

;; Make tool calls
(let ((connection (gethash "example" mcp-server-connections)))
  (mcp-async-call-tool 
   connection
   "example_tool"
   '(:param "value")
   (lambda (result) (message "Success: %s" result))
   (lambda (code msg) (message "Error: %s - %s" code msg))))
```