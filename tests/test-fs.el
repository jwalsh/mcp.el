;; Test filesystem operations

;; Set up load paths
(add-to-list 'load-path (expand-file-name "."))
(add-to-list 'load-path (expand-file-name "examples/filesystem"))

;; Load required packages
(require 'mcp)
(require 'mcp-fs-connection)
(require 'mcp-fs-list)
(require 'mcp-fs-read)

;; Connect to filesystem server
(message "Connecting to filesystem server...")
(mcp-fs-connect)

;; Wait a bit for connection
(sleep-for 2)
(message "Connected. Listing home directory...")

;; List home directory
(mcp-fs-list "~")

;; Wait for results
(sleep-for 2)
(message "Done.")