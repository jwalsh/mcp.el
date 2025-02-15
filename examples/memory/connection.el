;; Connection

;; [[file:../../servers.org::*Connection][Connection:1]]
;;; connection.el --- Memory MCP connection -*- lexical-binding: t -*-

;;; Commentary:
;; Basic connection setup for the MCP memory server

;;; Code:

(require 'mcp)

(defun mcp-memory-connect ()
  "Connect to the Memory MCP server."
  (interactive)
  (mcp-connect-server 
   "memory" 
   "npx" 
   (list "-y" "@modelcontextprotocol/server-memory")
   :initial-callback
   (lambda (connection)
     (message "Connected to %s" (jsonrpc-name connection)))))

(provide 'mcp-memory-connection)
;;; connection.el ends here
;; Connection:1 ends here
