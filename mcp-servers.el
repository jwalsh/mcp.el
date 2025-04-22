;;; mcp-servers.el --- MCP Server Management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jason Walsh

;; Author: Jason Walsh <jwalsh@example.com>
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "26.1") (jsonrpc "1.0.0") (mcp "0.1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a wrapper for managing MCP servers.
;; It automatically starts and manages multiple MCP servers based on a
;; claude_desktop_config.json file, similar to the Claude Desktop application.

;;; Code:

(require 'mcp)
(require 'json)

(defgroup mcp-servers nil
  "MCP Server Management."
  :group 'tools
  :prefix "mcp-servers-")

(defcustom mcp-servers-config-file nil
  "Path to the claude_desktop_config.json file.
If nil, the package will search for it in standard locations."
  :type '(choice (const :tag "Auto-detect" nil)
                (file :tag "Config file path"))
  :group 'mcp-servers)

(defcustom mcp-servers-auto-start nil
  "Whether to automatically start MCP servers when Emacs starts."
  :type 'boolean
  :group 'mcp-servers)

(defvar mcp-servers-active-processes nil
  "Alist of active MCP server processes: (name . process).")

(defvar mcp-servers-config nil
  "The loaded MCP server configuration.")

(defvar mcp-servers-connections nil
  "Alist of active MCP server connections: (name . connection).")

(defun mcp-servers-find-config-file ()
  "Find the claude_desktop_config.json file in standard locations."
  (or mcp-servers-config-file
      (let ((locations (list (expand-file-name "claude_desktop_config.json" default-directory)
                             (expand-file-name "dist/claude_desktop_config.json" default-directory)
                             (expand-file-name "~/.config/Claude/claude_desktop_config.json")
                             (expand-file-name "~/Library/Application Support/Claude/claude_desktop_config.json")
                             (expand-file-name "~/Library/Containers/com.anthropic.claude/Data/Library/Application Support/Claude/claude_desktop_config.json"))))
        (cl-find-if #'file-exists-p locations))))

(defun mcp-servers-load-config ()
  "Load the MCP server configuration from file."
  (let ((config-file (mcp-servers-find-config-file)))
    (unless config-file
      (error "Could not find MCP server configuration file"))
    
    (with-temp-buffer
      (insert-file-contents config-file)
      (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'string))
        (let ((config (json-read-from-string (buffer-string))))
          (setq mcp-servers-config config)
          (message "Loaded MCP server configuration from %s" config-file)
          config)))))

(defun mcp-servers-expand-vars (value)
  "Expand environment variables in VALUE."
  (cond
   ((stringp value)
    (let ((result value))
      (when (string-match "\\$\\([A-Za-z0-9_]+\\)" result)
        (let* ((var-name (match-string 1 result))
               (env-value (getenv var-name)))
          (when env-value
            (setq result (replace-regexp-in-string (concat "\\$" var-name) env-value result)))))
      result))
   ((listp value)
    (mapcar #'mcp-servers-expand-vars value))
   (t value)))

(defun mcp-servers-start-server (name config)
  "Start an MCP server with NAME using CONFIG."
  (let* ((command (gethash "command" config))
         (args (mcp-servers-expand-vars (gethash "args" config)))
         (env-config (gethash "env" config))
         (process-environment (copy-sequence process-environment))
         proc)
    
    ;; Add environment variables if specified
    (when env-config
      (maphash (lambda (key value)
                 (push (format "%s=%s" key (mcp-servers-expand-vars value)) process-environment))
               env-config))
    
    ;; Create buffer for process output
    (let ((buffer (get-buffer-create (format "*mcp-server-%s*" name))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (format "Starting MCP server '%s'...\n" name))
        (insert (format "Command: %s %s\n\n" command (mapconcat #'identity args " ")))
        
        ;; Start the process
        (setq proc (apply #'start-process 
                          (format "mcp-server-%s" name)
                          buffer
                          command
                          args))
        
        ;; Setup process properties
        (process-put proc 'server-name name)
        (set-process-sentinel proc #'mcp-servers-process-sentinel)
        
        ;; Add to active processes
        (push (cons name proc) mcp-servers-active-processes)
        
        ;; Return the process
        proc))))

(defun mcp-servers-process-sentinel (process event)
  "Handle PROCESS EVENT for MCP servers."
  (let ((name (process-get process 'server-name))
        (status (string-trim event)))
    (when (member status '("finished" "exited abnormally" "killed"))
      (message "MCP server '%s' has %s." name status)
      
      ;; Remove from active processes
      (setq mcp-servers-active-processes 
            (assoc-delete-all name mcp-servers-active-processes))
      
      ;; Remove from connections if exists
      (when (assoc name mcp-servers-connections)
        (let ((conn (cdr (assoc name mcp-servers-connections))))
          (when (jsonrpc-running-p conn)
            (jsonrpc-shutdown conn)))
        (setq mcp-servers-connections
              (assoc-delete-all name mcp-servers-connections))))))

(defun mcp-servers-connect-to-server (name process)
  "Create an MCP connection to server NAME running as PROCESS."
  (let* ((conn (make-instance 'mcp-process-connection
                             :name (format "MCP-%s" name)
                             :process process)))
    
    ;; Initialize the connection
    (mcp-async-initialize-message
     conn
     (lambda (_version server-info capabilities)
       (message "Connected to MCP server '%s'. Protocol version: %s" 
                name (gethash "protocolVersion" server-info))
       
       ;; List available tools
       (mcp-async-list-tools
        conn
        (lambda (_conn tools)
          (message "Server '%s' provides %d tools" name (length tools))))))
    
    ;; Store the connection
    (push (cons name conn) mcp-servers-connections)
    
    ;; Return the connection
    conn))

(defun mcp-servers-list-all-tools ()
  "List all tools available from all connected MCP servers."
  (interactive)
  (with-help-window "*MCP Tools*"
    (let ((tools-list (make-hash-table :test 'equal)))
      ;; Collect tools from all servers
      (dolist (server-conn mcp-servers-connections)
        (let* ((server-name (car server-conn))
               (conn (cdr server-conn))
               (server-tools (mcp--tools conn)))
          (when server-tools
            (dolist (tool server-tools)
              (let* ((tool-name (gethash "name" tool))
                     (key (format "%s" tool-name)))
                (puthash key (cons server-name tool) tools-list))))))
      
      ;; Display tools sorted by name
      (let ((sorted-keys (sort (hash-table-keys tools-list) #'string<)))
        (if sorted-keys
            (progn
              (princ "Available MCP Tools:\n\n")
              (dolist (key sorted-keys)
                (let* ((server-tool (gethash key tools-list))
                       (server-name (car server-tool))
                       (tool (cdr server-tool))
                       (name (gethash "name" tool))
                       (desc (or (gethash "description" tool) "No description available")))
                  (princ (format "  %s [%s]\n    %s\n\n" name server-name desc)))))
          (princ "No MCP tools available. Make sure servers are running and connected."))))))

(defun mcp-servers-get-connection (server-name)
  "Get the MCP connection for SERVER-NAME."
  (cdr (assoc server-name mcp-servers-connections)))

(defun mcp-servers-call-tool (server-name tool-name &optional arguments)
  "Call TOOL-NAME on SERVER-NAME with ARGUMENTS."
  (interactive
   (let* ((servers (mapcar #'car mcp-servers-connections))
          (server-name (completing-read "Server: " servers nil t))
          (conn (mcp-servers-get-connection server-name))
          (tools (mapcar (lambda (tool) (gethash "name" tool)) (mcp--tools conn)))
          (tool-name (completing-read "Tool: " tools nil t))
          (args-str (read-string "Arguments (JSON): " "{}"))
          (arguments (json-read-from-string args-str)))
     (list server-name tool-name arguments)))
  
  (let ((conn (mcp-servers-get-connection server-name)))
    (unless conn
      (error "No connection to server '%s'" server-name))
    
    (message "Calling %s on server %s..." tool-name server-name)
    (mcp-async-call-tool 
     conn tool-name arguments
     (lambda (result)
       (message "Tool call completed")
       (with-help-window (format "*MCP Tool Result: %s/%s*" server-name tool-name)
         (princ (format "Result from %s/%s:\n\n" server-name tool-name))
         (let ((json-encoding-pretty-print t))
           (princ (json-encode result)))))
     (lambda (code message)
       (message "Tool call failed: %s - %s" code message)))))

;;;###autoload
(defun mcp-servers-start-all ()
  "Start all MCP servers defined in the configuration."
  (interactive)
  (unless mcp-servers-config
    (mcp-servers-load-config))
  
  (let ((servers (gethash "mcpServers" mcp-servers-config)))
    (if servers
        (progn
          (message "Starting %d MCP servers..." (hash-table-count servers))
          (maphash (lambda (name config)
                     (message "Starting MCP server '%s'..." name)
                     (let ((proc (mcp-servers-start-server name config)))
                       ;; Wait a bit for the server to start before connecting
                       (run-with-timer 1 nil 
                                       (lambda (n p)
                                         (when (process-live-p p)
                                           (mcp-servers-connect-to-server n p)))
                                       name proc)))
                   servers)
          (message "All MCP servers started."))
      (message "No MCP servers defined in configuration."))))

;;;###autoload
(defun mcp-servers-stop-all ()
  "Stop all running MCP servers."
  (interactive)
  (dolist (conn mcp-servers-connections)
    (when (jsonrpc-running-p (cdr conn))
      (jsonrpc-shutdown (cdr conn))))
  
  (setq mcp-servers-connections nil)
  
  (dolist (proc mcp-servers-active-processes)
    (when (process-live-p (cdr proc))
      (kill-process (cdr proc))))
  
  (setq mcp-servers-active-processes nil)
  (message "All MCP servers stopped."))

;;;###autoload
(defun mcp-servers-restart-all ()
  "Restart all MCP servers."
  (interactive)
  (mcp-servers-stop-all)
  ;; Wait a bit before restarting
  (run-with-timer 1 nil #'mcp-servers-start-all))

;;;###autoload
(defun mcp-servers-status ()
  "Show status of all MCP servers."
  (interactive)
  (with-help-window "*MCP Servers Status*"
    (princ "MCP Servers Status:\n\n")
    
    (if mcp-servers-active-processes
        (dolist (proc mcp-servers-active-processes)
          (let* ((name (car proc))
                 (process (cdr proc))
                 (status (process-status process))
                 (conn (assoc name mcp-servers-connections))
                 (connected (and conn (jsonrpc-running-p (cdr conn)))))
            (princ (format "  Server: %s\n" name))
            (princ (format "    Process: %s (PID: %s)\n" 
                          (if (process-live-p process) "RUNNING" "TERMINATED")
                          (process-id process)))
            (princ (format "    Status: %s\n" status))
            (princ (format "    Connection: %s\n" 
                          (if connected "ESTABLISHED" "NOT CONNECTED")))
            (princ "\n")))
      (princ "  No MCP servers are currently running.\n"))
    
    (princ "\nUse M-x mcp-servers-start-all to start all configured servers.")))

;; Define keybindings for the MCP servers management
(defvar mcp-servers-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'mcp-servers-start-all)
    (define-key map (kbd "S") #'mcp-servers-stop-all)
    (define-key map (kbd "r") #'mcp-servers-restart-all)
    (define-key map (kbd "t") #'mcp-servers-list-all-tools)
    (define-key map (kbd "c") #'mcp-servers-call-tool)
    (define-key map (kbd "?") #'mcp-servers-status)
    map)
  "Keymap for MCP servers management commands.")

;; Expose prefix keymap
(defcustom mcp-servers-keymap-prefix "C-c m"
  "Prefix for mcp-servers keymap."
  :type 'string
  :group 'mcp-servers)

;;;###autoload
(defun mcp-servers-setup-keymap ()
  "Setup keymap for mcp-servers."
  (when mcp-servers-keymap-prefix
    (let ((keymap (when (boundp 'mcp-mode-map) mcp-mode-map)))
      (when keymap 
        (define-key keymap (kbd mcp-servers-keymap-prefix) mcp-servers-map)))))

;;;###autoload
(define-minor-mode mcp-servers-mode
  "Minor mode for managing MCP servers based on Claude Desktop configuration."
  :lighter " MCP-Servers"
  :keymap mcp-servers-map
  :global t
  (if mcp-servers-mode
      (progn
        (mcp-servers-setup-keymap)
        (when mcp-servers-auto-start
          (mcp-servers-start-all)))
    (mcp-servers-stop-all)))

;; Auto-start when enabled
(when mcp-servers-auto-start
  (add-hook 'after-init-hook #'mcp-servers-start-all))

(provide 'mcp-servers)
;;; mcp-servers.el ends here