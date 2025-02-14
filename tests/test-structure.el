;; Set up the load path for MCP
(add-to-list 'load-path (expand-file-name "."))
(add-to-list 'load-path (expand-file-name "examples/filesystem"))
(add-to-list 'load-path (expand-file-name "examples/memory"))

;; Load the main MCP package
(require 'mcp)

;; Now try to load the example modules
(condition-case err
    (progn
      (require 'mcp-fs-connection)
      (message "Successfully loaded filesystem connection"))
  (error (message "Failed to load filesystem connection: %S" err)))

(condition-case err
    (progn
      (require 'mcp-memory-connection)
      (message "Successfully loaded memory connection"))
  (error (message "Failed to load memory connection: %S" err)))