;;; ob-mcp.el --- Org Babel functions for MCP -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jason Walsh

;; Author: Your Name
;; Keywords: literate programming, reproducible research, MCP
;; Homepage: https://github.com/jwalsh/ob-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4") (jsonrpc "1.0.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides org-babel support for Model Context Protocol (MCP) servers.
;; This extension allows execution of MCP commands directly from org-mode code blocks.

;;; Code:
(require 'ob)
(require 'org)
(require 'jsonrpc)
(require 'cl-lib)

(defvar ob-mcp-servers nil
  "Alist of MCP server connections.
Each entry is of the form (SERVER-NAME . CONNECTION).")

(defvar ob-mcp-default-server nil
  "Default MCP server to use when none is specified.")

(defvar ob-mcp-server-config nil
  "Configuration for MCP servers.
This should be a JSON object like the one in Claude Desktop.")

(defvar ob-mcp-timeout 10
  "Timeout in seconds for MCP requests.")

(defun ob-mcp-load-config (config-file)
  "Load MCP server configuration from CONFIG-FILE."
  (interactive "fMCP config file: ")
  (when (file-exists-p config-file)
    (with-temp-buffer
      (insert-file-contents config-file)
      (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'string))
        (setq ob-mcp-server-config (json-read))))))

(defun ob-mcp-load-config-from-string (config-string)
  "Load MCP server configuration from CONFIG-STRING."
  (interactive "sMCP config JSON: ")
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (setq ob-mcp-server-config (json-read-from-string config-string))))

(defun ob-mcp-expand-env-vars (value)
  "Expand environment variables in VALUE if it's a string."
  (if (stringp value)
      (replace-regexp-in-string
       "\\$\\([A-Za-z0-9_]+\\)\\|\\${\\([A-Za-z0-9_]+\\)}"
       (lambda (match)
         (let ((var (match-string 1 match)))
           (unless var
             (setq var (match-string 2 match)))
           (or (getenv var) match)))
       value t t)
    value))

(defun ob-mcp-expand-paths (value)
  "Expand paths in VALUE if it's a string."
  (if (stringp value)
      (let ((expanded (expand-file-name value)))
        (if (file-exists-p expanded)
            expanded
          value))
    value))

(defun ob-mcp-process-config-value (value)
  "Process VALUE in config by expanding env vars and paths."
  (ob-mcp-expand-paths (ob-mcp-expand-env-vars value)))

(defun ob-mcp-connect-server (server-name)
  "Connect to MCP SERVER-NAME defined in the config."
  (interactive
   (list (completing-read "MCP Server: "
                         (when ob-mcp-server-config
                           (hash-table-keys
                            (gethash "mcpServers" ob-mcp-server-config))))))
  
  (when (and ob-mcp-server-config
             (not (hash-table-p (gethash "mcpServers" ob-mcp-server-config))))
    (error "Invalid MCP server configuration"))
  
  (let* ((servers (gethash "mcpServers" ob-mcp-server-config))
         (server-config (gethash server-name servers)))
    
    (unless server-config
      (error "MCP server %s not found in configuration" server-name))
    
    (let* ((command (ob-mcp-process-config-value (gethash "command" server-config)))
           (args (mapcar #'ob-mcp-process-config-value (gethash "args" server-config)))
           (env (when (gethash "env" server-config)
                  (let ((result nil))
                    (maphash (lambda (k v)
                               (push (cons k (ob-mcp-process-config-value v))
                                     result))
                             (gethash "env" server-config))
                    result)))
           (connection (make-instance 'jsonrpc-process-connection
                                      :name server-name
                                      :process (make-process
                                               :name (format "mcp-%s" server-name)
                                               :command (cons command args)
                                               :connection-type 'pipe
                                               :coding 'utf-8-unix
                                               :stderr (get-buffer-create
                                                        (format "*mcp-%s-stderr*" server-name))
                                               :noquery t
                                               :environment env))))
      
      ;; Wait for the server to start
      (sleep-for 1)
      
      ;; Store the connection
      (setf (alist-get server-name ob-mcp-servers nil nil #'equal) connection)
      
      ;; Set as default if not yet set
      (unless ob-mcp-default-server
        (setq ob-mcp-default-server server-name))
      
      connection)))

(defun ob-mcp-get-connection (server-name)
  "Get or create connection to MCP SERVER-NAME."
  (or (alist-get server-name ob-mcp-servers nil nil #'equal)
      (ob-mcp-connect-server server-name)))

(defun ob-mcp-disconnect-server (server-name)
  "Disconnect from MCP SERVER-NAME."
  (interactive
   (list (completing-read "MCP Server: "
                         (mapcar #'car ob-mcp-servers))))
  
  (let ((connection (alist-get server-name ob-mcp-servers nil nil #'equal)))
    (when connection
      (jsonrpc-shutdown connection)
      (setf (alist-get server-name ob-mcp-servers nil t #'equal) nil)
      
      ;; Clear default if it was this one
      (when (equal server-name ob-mcp-default-server)
        (setq ob-mcp-default-server nil)))))

(defun ob-mcp-disconnect-all ()
  "Disconnect from all MCP servers."
  (interactive)
  (dolist (server ob-mcp-servers)
    (when (cdr server)
      (jsonrpc-shutdown (cdr server))))
  (setq ob-mcp-servers nil
        ob-mcp-default-server nil))

(defun ob-mcp-execute (server-name command &optional timeout)
  "Execute COMMAND on MCP SERVER-NAME with optional TIMEOUT."
  (let* ((connection (ob-mcp-get-connection server-name))
         (timeout (or timeout ob-mcp-timeout))
         (response (jsonrpc-request connection
                                   "execute"
                                   `(:command ,command)
                                   :timeout timeout)))
    response))

(defun ob-mcp-get-tools (server-name)
  "Get available tools from MCP SERVER-NAME."
  (ob-mcp-execute server-name "tools/list"))

;; Org Babel integration
(defun org-babel-execute:mcp (body params)
  "Execute MCP commands in BODY according to PARAMS."
  (let* ((server (or (cdr (assq :server params))
                    ob-mcp-default-server
                    (error "No MCP server specified")))
         (timeout (or (cdr (assq :timeout params))
                     ob-mcp-timeout))
         (vars (org-babel--get-vars params))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         ;; Process the body, expanding variables
         (processed-body (org-babel-expand-body:generic body params vars)))
    
    ;; Split commands by lines and execute each one
    (let ((commands (split-string processed-body "\n" t "[ \t]+")))
      (if (= (length commands) 1)
          ;; Single command - return direct result
          (let ((response (ob-mcp-execute server (car commands) timeout)))
            (cond
             ((member "file" result-params)
              ;; Process file output if requested
              (let ((file (cdr (assq :file params))))
                (when file
                  ;; Here you'd save the output to the file
                  ;; This depends on the specific MCP server type
                  ;; For QR code, this might be binary data
                  ;; For now we just return the file name
                  file)))
             
             ((member "raw" result-params)
              ;; Return raw output
              response)
             
             ((member "drawer" result-params)
              ;; Format as drawer
              (format ":RESULTS:\n%s\n:END:" response))
             
             ((or (member "table" result-params)
                 (eq result-type 'table))
              ;; Try to interpret as a table
              (condition-case nil
                  (if (stringp response)
                      (with-temp-buffer
                        (insert response)
                        (org-table-convert-region (point-min) (point-max))
                        (buffer-string))
                    response)
                (error response)))
             
             (t 
              ;; Default string output
              (if (stringp response)
                  response
                (format "%S" response)))))
        
        ;; Multiple commands - execute in sequence and collect results
        (let ((results (mapcar (lambda (cmd)
                                (ob-mcp-execute server cmd timeout))
                              commands)))
          (mapconcat (lambda (result)
                       (if (stringp result)
                           result
                         (format "%S" result)))
                     results
                     "\n"))))))

(add-to-list 'org-babel-load-languages '(mcp . t))
(provide 'ob-mcp)
;;; ob-mcp.el ends here
