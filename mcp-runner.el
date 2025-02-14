;;; mcp-runner.el --- MCP Server Runner -*- lexical-binding: t -*-

;;; Commentary:
;; Runner for MCP servers providing unified interface for all server types.
;; Manages connections, tools, and provides interactive functions.

;;; Code:

(require 'mcp)

;; Custom group for MCP
(defgroup mcp-runner nil
  "MCP server runner configuration."
  :group 'tools
  :prefix "mcp-runner-")

(defcustom mcp-runner-epub-dir "~/epub/"
  "Default directory for EPUB files."
  :type 'directory
  :group 'mcp-runner)

;; Server management
(defun mcp-runner-start-all ()
  "Start all configured MCP servers."
  (interactive)
  ;; Filesystem server
  (mcp-connect-server 
   "filesystem" 
   "npx" 
   (list "-y" 
         "@modelcontextprotocol/server-filesystem" 
         (expand-file-name mcp-runner-epub-dir))
   :initial-callback
   (lambda (connection)
     (message "Connected to filesystem server"))
   :tools-callback
   (lambda (connection tools)
     (message "Filesystem tools loaded: %d" (length tools)))
   :prompts-callback
   (lambda (connection prompts)
     (when prompts
       (message "Filesystem prompts loaded: %d" (length prompts))))
   :resources-callback
   (lambda (connection resources)
     (when resources
       (message "Filesystem resources loaded: %d" (length resources)))))

  ;; Git server
  (mcp-connect-server 
   "git" 
   "npx" 
   (list "-y" "mcp-server-git")
   :initial-callback
   (lambda (connection)
     (message "Connected to git server")))

  ;; Time server
  (mcp-connect-server 
   "time" 
   "npx" 
   (list "-y" "mcp-server-time")
   :initial-callback
   (lambda (connection)
     (message "Connected to time server")))

  ;; Memory server
  (mcp-connect-server 
   "memory" 
   "npx" 
   (list "-y" "@modelcontextprotocol/server-memory")
   :initial-callback
   (lambda (connection)
     (message "Connected to memory server"))))

(defun mcp-runner-stop-all ()
  "Stop all running MCP servers."
  (interactive)
  (dolist (server '("filesystem" "git" "time" "memory"))
    (when (gethash server mcp-server-connections)
      (mcp-stop-server server)
      (message "Stopped %s server" server))))

;; Key bindings
(defvar mcp-runner-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m s") 'mcp-runner-start-all)
    (define-key map (kbd "C-c m q") 'mcp-runner-stop-all)
    ;; Filesystem
    (define-key map (kbd "C-c m f l") 'mcp-fs-list)
    (define-key map (kbd "C-c m f r") 'mcp-fs-read)
    ;; Git
    (define-key map (kbd "C-c m g s") 'mcp-git-status)
    (define-key map (kbd "C-c m g l") 'mcp-git-log)
    ;; Memory
    (define-key map (kbd "C-c m m s") 'mcp-memory-search)
    (define-key map (kbd "C-c m m e") 'mcp-memory-create-entity)
    ;; Time
    (define-key map (kbd "C-c m t c") 'mcp-time-current)
    (define-key map (kbd "C-c m t v") 'mcp-time-convert)
    map)
  "Keymap for MCP runner mode.")

;;;###autoload
(define-minor-mode mcp-runner-mode
  "Minor mode for running MCP servers.
\\{mcp-runner-mode-map}"
  :lighter " MCP"
  :keymap mcp-runner-mode-map
  :global t
  (if mcp-runner-mode
      (progn
        (mcp-runner-start-all)
        (message "MCP runner mode enabled"))
    (mcp-runner-stop-all)
    (message "MCP runner mode disabled")))

;; Helper functions
(defun mcp-runner-status ()
  "Show status of all MCP servers."
  (interactive)
  (with-current-buffer (get-buffer-create "*MCP Status*")
    (erase-buffer)
    (insert "MCP Servers Status\n")
    (insert "=================\n\n")
    (dolist (server '("filesystem" "git" "time" "memory"))
      (let ((connection (gethash server mcp-server-connections)))
        (insert (format "%s: %s\n" 
                       server 
                       (if connection "Connected" "Not connected")))))
    (display-buffer (current-buffer))))

;; Auto-start option
(defcustom mcp-runner-auto-start nil
  "Whether to start MCP servers automatically when Emacs starts."
  :type 'boolean
  :group 'mcp-runner)

(when mcp-runner-auto-start
  (add-hook 'after-init-hook #'mcp-runner-mode))

(provide 'mcp-runner)
;;; mcp-runner.el ends here
