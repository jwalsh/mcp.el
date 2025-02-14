;; Test basic MCP functionality

;; Load path setup
(add-to-list 'load-path (expand-file-name "examples/filesystem"))
(add-to-list 'load-path (expand-file-name "examples/memory"))

;; Load the examples
(require 'mcp-fs-connection)
(require 'mcp-fs-list)
(require 'mcp-fs-read)

(require 'mcp-memory-connection)
(require 'mcp-memory-entity)
(require 'mcp-memory-search)

;; Run basic tests
(mcp-fs-connect)
(mcp-memory-connect)

;; Test filesystem operations
(mcp-fs-list "~")
(mcp-fs-read "servers.org")

;; Test memory operations
(mcp-memory-create-entity "test-entity" "test" "observation1,observation2")
(mcp-memory-search "test")