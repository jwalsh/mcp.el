;; Test MCP examples

;; Load path setup
(add-to-list 'load-path (expand-file-name "examples/filesystem"))
(add-to-list 'load-path (expand-file-name "examples/git"))
(add-to-list 'load-path (expand-file-name "examples/memory"))
(add-to-list 'load-path (expand-file-name "examples/time"))

;; Load the examples
(require 'mcp-fs-connection)
(require 'mcp-fs-list)
(require 'mcp-fs-read)

(require 'mcp-git-connection)
(require 'mcp-git-status)
(require 'mcp-git-log)

(require 'mcp-memory-connection)
(require 'mcp-memory-entity)
(require 'mcp-memory-search)

(require 'mcp-time-connection)
(require 'mcp-time-current)
(require 'mcp-time-convert)

;; Run some basic tests
(mcp-fs-connect)
(mcp-git-connect)
(mcp-memory-connect)
(mcp-time-connect)

;; Test filesystem operations
(mcp-fs-list "~")
(mcp-fs-read "servers.org")

;; Test git operations
(mcp-git-status ".")
(mcp-git-log "." 5)

;; Test memory operations
(mcp-memory-create-entity "test-entity" "test" "observation1,observation2")
(mcp-memory-search "test")

;; Test time operations
(mcp-time-current "UTC")
(mcp-time-convert "12:00" "UTC" "America/New_York")