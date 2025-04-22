;;; mcp-servers-example.el --- Example MCP Servers Setup -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jason Walsh
;;
;; Author: Jason Walsh <jwalsh@example.com>
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides an example of how to use the mcp-servers.el package
;; to create an Emacs setup similar to Claude Desktop with MCP servers.

;;; Code:

(require 'mcp)
(require 'mcp-servers)

;; 1. Set the configuration file (optional, will auto-detect if not set)
;; (setq mcp-servers-config-file "/path/to/claude_desktop_config.json")

;; 2. Auto-start MCP servers on Emacs startup (optional)
(setq mcp-servers-auto-start t)

;; 3. Enable the mcp-servers mode to manage the servers
(mcp-servers-mode 1)

;; 4. Example of how to interact with MCP tools

;; Define a function to create a QR code from a URL
(defun generate-qrcode (url)
  "Generate a QR code for the given URL using the MCP qrcode server."
  (interactive "sURL for QR code: ")
  (let ((server-name "qrcode")
        (tool-name "generate-qrcode")
        (arguments (list :content url :format "text")))
    (let ((conn (mcp-servers-get-connection server-name)))
      (if conn
          (progn
            (message "Generating QR code for %s..." url)
            (mcp-async-call-tool 
             conn tool-name arguments
             (lambda (result)
               (with-help-window "*QR Code*"
                 (let* ((content (gethash "content" result))
                        (text (aref content 0)))
                   (princ (gethash "text" text)))))
             (lambda (code message)
               (message "Failed to generate QR code: %s - %s" code message))))
        (message "QR code server is not running. Start it with M-x mcp-servers-start-all")))))

;; Define a function to browse filesystem using MCP
(defun browse-mcp-fs (path)
  "Browse filesystem at PATH using MCP filesystem server."
  (interactive "DPath to browse: ")
  (let ((server-name "filesystem")
        (tool-name "list-files")
        (arguments (list :path (expand-file-name path) :maxDepth 1)))
    (let ((conn (mcp-servers-get-connection server-name)))
      (if conn
          (progn
            (message "Listing files in %s..." path)
            (mcp-async-call-tool 
             conn tool-name arguments
             (lambda (result)
               (with-help-window "*MCP Filesystem*"
                 (princ (format "Contents of %s:\n\n" path))
                 (let* ((items (gethash "items" result)))
                   (dolist (item items)
                     (let* ((name (gethash "name" item))
                            (type (gethash "type" item))
                            (size (gethash "size" item)))
                       (princ (format "%-20s  %-10s  %s\n" 
                                     name 
                                     (if (equal type "directory") "<DIR>" 
                                       (format "%d bytes" size))
                                     type)))))))
             (lambda (code message)
               (message "Failed to list files: %s - %s" code message))))
        (message "Filesystem server is not running. Start it with M-x mcp-servers-start-all")))))

;; Define a git status function using MCP
(defun git-status-mcp (repo-path)
  "Show git status for REPO-PATH using MCP git server."
  (interactive "DGit repository path: ")
  (let ((server-name "git")
        (tool-name "status")
        (arguments (list :cwd (expand-file-name repo-path))))
    (let ((conn (mcp-servers-get-connection server-name)))
      (if conn
          (progn
            (message "Getting git status for %s..." repo-path)
            (mcp-async-call-tool 
             conn tool-name arguments
             (lambda (result)
               (with-help-window "*MCP Git Status*"
                 (princ (format "Git status for %s:\n\n" repo-path))
                 (let* ((branch (gethash "currentBranch" result))
                        (files (gethash "files" result)))
                   (princ (format "Branch: %s\n\n" branch))
                   (princ "Changed files:\n")
                   (if (> (length files) 0)
                       (dolist (file files)
                         (let* ((path (gethash "path" file))
                                (status (gethash "status" file)))
                           (princ (format "  %s: %s\n" status path))))
                     (princ "  No changes\n")))))
             (lambda (code message)
               (message "Failed to get git status: %s - %s" code message))))
        (message "Git server is not running. Start it with M-x mcp-servers-start-all")))))

;; Define keybindings for the example functions
(global-set-key (kbd "C-c q") 'generate-qrcode)
(global-set-key (kbd "C-c f") 'browse-mcp-fs)
(global-set-key (kbd "C-c g") 'git-status-mcp)

;; Instructions for users
(defun mcp-servers-quick-help ()
  "Show quick help for MCP servers."
  (interactive)
  (with-help-window "*MCP Servers Help*"
    (princ "MCP Servers Quick Help\n")
    (princ "====================\n\n")
    (princ "Server Management:\n")
    (princ "  C-c m s    Start all MCP servers\n")
    (princ "  C-c m S    Stop all MCP servers\n") 
    (princ "  C-c m r    Restart all MCP servers\n")
    (princ "  C-c m ?    Show MCP servers status\n")
    (princ "  C-c m t    List all available MCP tools\n")
    (princ "  C-c m c    Call an MCP tool interactively\n\n")
    
    (princ "Example Functions:\n")
    (princ "  C-c q      Generate QR code\n")
    (princ "  C-c f      Browse filesystem\n")
    (princ "  C-c g      Show git status\n\n")
    
    (princ "To get started, first run C-c m s to start all configured MCP servers.\n")
    (princ "Then use C-c m t to see what tools are available from the servers.\n")))

(global-set-key (kbd "C-c m h") 'mcp-servers-quick-help)

;; Show help when loaded
(run-with-idle-timer 2 nil 'mcp-servers-quick-help)

(provide 'mcp-servers-example)
;;; mcp-servers-example.el ends here