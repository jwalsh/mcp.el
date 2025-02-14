(require 'mcp)

;; Initialize a filesystem MCP server connection
(mcp-connect-server "filesystem" 
                    "npx" 
                    '("-y" "@modelcontextprotocol/server-filesystem" "~/Documents/")
                    :initial-callback
                    (lambda (connection)
                      (message "Connected to %s" (jsonrpc-name connection)))
                    :tools-callback
                    (lambda (connection tools)
                      (message "Available tools: %s" tools)))

;; Create a file writing tool
(mcp-make-text-gptel-tool "filesystem" "write_file")

;; Use the tool synchronously
(let ((connection (gethash "filesystem" mcp-server-connections)))
  (mcp-call-tool "write_file" 
                 '(:path "example.txt" 
                   :content "Hello, MCP!")))
