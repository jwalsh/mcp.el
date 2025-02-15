;; Connection

;; [[file:../../servers.org::*Connection][Connection:1]]
;;; connection.el --- Filesystem MCP connection -*- lexical-binding: t -*-

;;; Commentary:
;; Basic connection setup for the MCP filesystem server

;;; Code:

(require 'mcp)

(defun mcp-fs-connect ()
  "Connect to the filesystem MCP server."
  (interactive)
  (mcp-connect-server 
   "filesystem" 
   "npx" 
   (list "-y" 
         "@modelcontextprotocol/server-filesystem" 
         (expand-file-name "~/epub/"))
   :initial-callback
   (lambda (connection)
     (message "Connected to %s" (jsonrpc-name connection)))))

(provide 'mcp-fs-connection)
;;; connection.el ends here
;; Connection:1 ends here
