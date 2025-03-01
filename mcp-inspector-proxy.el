;; mcp-inspector-proxy.el - Connect to MCP Inspector proxy from Emacs

;;; Code:
(require 'url)
(require 'json)
(require 'sse)  ;; Server-sent events package - install with M-x package-install sse
(require 'websocket)  ;; WebSocket package - install with M-x package-install websocket

(defgroup mcp-inspector-proxy nil
  "MCP Inspector proxy integration for Emacs."
  :group 'tools
  :prefix "mcp-inspector-proxy-")

(defcustom mcp-inspector-proxy-host "localhost"
  "Host for the MCP Inspector proxy server."
  :type 'string
  :group 'mcp-inspector-proxy)

(defcustom mcp-inspector-proxy-port 3000
  "Port for the MCP Inspector proxy server."
  :type 'integer
  :group 'mcp-inspector-proxy)

(defcustom mcp-inspector-proxy-output-buffer "*MCP Inspector Proxy*"
  "Buffer name for MCP Inspector proxy output."
  :type 'string
  :group 'mcp-inspector-proxy)

(defvar mcp-inspector-proxy-session-id nil
  "Current session ID for the MCP Inspector proxy connection.")

(defvar mcp-inspector-proxy-sse-client nil
  "SSE client for MCP Inspector proxy events.")

(defvar mcp-inspector-proxy-ws-client nil
  "WebSocket client for MCP Inspector proxy communication.")

;; Core functionality
(defun mcp-inspector-proxy-url (path &optional params)
  "Create a URL for the given PATH with optional query PARAMS."
  (let ((url (format "http://%s:%d%s" 
                     mcp-inspector-proxy-host 
                     mcp-inspector-proxy-port 
                     path)))
    (if params
        (concat url "?" (url-build-query-string params))
      url)))

(defun mcp-inspector-proxy-get-buffer ()
  "Get or create the MCP Inspector proxy buffer."
  (let ((buffer (get-buffer-create mcp-inspector-proxy-output-buffer)))
    (with-current-buffer buffer
      (special-mode)  ;; Use special-mode for read-only buffer with keybindings
      (let ((inhibit-read-only t))
        (when (= (point-min) (point-max))  ;; Only insert header if buffer is empty
          (insert "# MCP Inspector Proxy Connection\n\n")
          (insert "Use [C-c m r] to send commands, [C-c m q] to disconnect\n\n"))))
    buffer))

(defun mcp-inspector-proxy-log (message)
  "Log MESSAGE to the MCP Inspector proxy buffer."
  (with-current-buffer (mcp-inspector-proxy-get-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "[%s] %s\n" 
                      (format-time-string "%H:%M:%S")
                      message)))))

(defun mcp-inspector-proxy-handle-sse-message (event)
  "Handle SSE EVENT from the MCP Inspector proxy."
  (let ((data (sse-event-data event))
        (event-type (sse-event-event event)))
    (when data
      (condition-case nil
          (let* ((json-data (json-read-from-string data))
                 (type (alist-get 'type json-data nil nil 'equal))
                 (payload (alist-get 'payload json-data nil nil 'equal)))
            (mcp-inspector-proxy-log (format "Event: %s" type))
            (cond
             ((string= type "connected")
              (mcp-inspector-proxy-log (format "Connected to MCP server")))
             ((string= type "output")
              (let ((output (alist-get 'text payload nil nil 'equal)))
                (mcp-inspector-proxy-log (format "Output: %s" output))))
             ((string= type "error")
              (let ((error-msg (alist-get 'text payload nil nil 'equal)))
                (mcp-inspector-proxy-log (format "Error: %s" error-msg))))
             (t (mcp-inspector-proxy-log (format "Unknown event: %s" data)))))
        (error (mcp-inspector-proxy-log (format "Failed to parse event: %s" data)))))))

(defun mcp-inspector-proxy-connect-sse ()
  "Connect to the MCP Inspector proxy SSE endpoint."
  (when mcp-inspector-proxy-sse-client
    (sse-close mcp-inspector-proxy-sse-client))
  
  (setq mcp-inspector-proxy-sse-client
        (sse-client
         (mcp-inspector-proxy-url "/events" `((sessionId . ,mcp-inspector-proxy-session-id)))
         :on-message #'mcp-inspector-proxy-handle-sse-message
         :on-open (lambda ()
                    (mcp-inspector-proxy-log "SSE connection established"))
         :on-error (lambda (err)
                     (mcp-inspector-proxy-log (format "SSE error: %s" err)))
         :on-close (lambda ()
                     (mcp-inspector-proxy-log "SSE connection closed")))))

(defun mcp-inspector-proxy-send-command (command)
  "Send COMMAND to the MCP Inspector proxy."
  (unless mcp-inspector-proxy-session-id
    (error "Not connected to MCP Inspector proxy"))
  
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data 
         (encode-coding-string
          (json-encode
           `((sessionId . ,mcp-inspector-proxy-session-id)
             (command . ,command)))
          'utf-8)))
    (url-retrieve
     (mcp-inspector-proxy-url "/send")
     (lambda (_status)
       (goto-char (point-min))
       (re-search-forward "^$" nil t)
       (let* ((json-string (buffer-substring (1+ (point)) (point-max)))
              (response (json-read-from-string json-string)))
         (if (eq (alist-get 'success response) t)
             (mcp-inspector-proxy-log (format "Command sent: %s" command))
           (mcp-inspector-proxy-log (format "Failed to send command: %s" command)))))
     nil t t)))

(defun mcp-inspector-proxy-initialize-session ()
  "Initialize a new session with the MCP Inspector proxy."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data
         (encode-coding-string
          (json-encode
           `((transportType . "stdio")
             (command . "node")
             (args . "build/direct-server.js")
             (env . ,(json-encode (getenv-internal)))))
          'utf-8)))
    (url-retrieve
     (mcp-inspector-proxy-url "/session")
     (lambda (_status)
       (goto-char (point-min))
       (re-search-forward "^$" nil t)
       (let* ((json-string (buffer-substring (1+ (point)) (point-max)))
              (response (json-read-from-string json-string)))
         (if (alist-get 'sessionId response nil nil 'equal)
             (progn
               (setq mcp-inspector-proxy-session-id (alist-get 'sessionId response))
               (mcp-inspector-proxy-log 
                (format "Session initialized with ID: %s" mcp-inspector-proxy-session-id))
               (mcp-inspector-proxy-connect-sse))
           (mcp-inspector-proxy-log "Failed to initialize session"))))
     nil t t)))

;; Interactive commands
(defun mcp-inspector-proxy-connect ()
  "Connect to the MCP Inspector proxy."
  (interactive)
  (let ((buffer (mcp-inspector-proxy-get-buffer)))
    (display-buffer buffer)
    (mcp-inspector-proxy-log "Connecting to MCP Inspector proxy...")
    (mcp-inspector-proxy-initialize-session)))

(defun mcp-inspector-proxy-disconnect ()
  "Disconnect from the MCP Inspector proxy."
  (interactive)
  (when mcp-inspector-proxy-sse-client
    (sse-close mcp-inspector-proxy-sse-client)
    (setq mcp-inspector-proxy-sse-client nil))
  
  (when mcp-inspector-proxy-session-id
    (let ((url-request-method "DELETE"))
      (url-retrieve
       (mcp-inspector-proxy-url (format "/session/%s" mcp-inspector-proxy-session-id))
       (lambda (_status)
         (mcp-inspector-proxy-log "Disconnected from MCP Inspector proxy")
         (setq mcp-inspector-proxy-session-id nil))
       nil t t))))

(defun mcp-inspector-proxy-send-command-interactive ()
  "Interactively send a command to the MCP Inspector proxy."
  (interactive)
  (if mcp-inspector-proxy-session-id
      (let ((command (read-string "MCP Command: ")))
        (mcp-inspector-proxy-send-command command))
    (message "Not connected to MCP Inspector proxy")))

(defun mcp-inspector-proxy-tools-list ()
  "List available tools from the MCP Inspector proxy."
  (interactive)
  (if mcp-inspector-proxy-session-id
      (mcp-inspector-proxy-send-command "tools/list")
    (message "Not connected to MCP Inspector proxy")))

(defun mcp-inspector-proxy-generate-qrcode (content)
  "Generate a QR code with the given CONTENT."
  (interactive "sQR Code content: ")
  (if mcp-inspector-proxy-session-id
      (mcp-inspector-proxy-send-command 
       (format "tools/call generate-qrcode --content=\"%s\" --size=5" content))
    (message "Not connected to MCP Inspector proxy")))

;; Keybindings and mode setup
(defvar mcp-inspector-proxy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m c") 'mcp-inspector-proxy-connect)
    (define-key map (kbd "C-c m q") 'mcp-inspector-proxy-disconnect)
    (define-key map (kbd "C-c m r") 'mcp-inspector-proxy-send-command-interactive)
    (define-key map (kbd "C-c m t") 'mcp-inspector-proxy-tools-list)
    (define-key map (kbd "C-c m g") 'mcp-inspector-proxy-generate-qrcode)
    map)
  "Keymap for MCP Inspector proxy mode.")

(define-minor-mode mcp-inspector-proxy-mode
  "Minor mode for interacting with MCP Inspector proxy."
  :init-value nil
  :lighter " MCP-Proxy"
  :keymap mcp-inspector-proxy-mode-map
  :group 'mcp-inspector-proxy)

;; Org-mode integration
(defun org-babel-execute:mcp-proxy (body params)
  "Execute MCP commands via proxy from org-babel.
BODY contains the commands to execute.
PARAMS are the babel execution parameters."
  (unless mcp-inspector-proxy-session-id
    (mcp-inspector-proxy-initialize-session)
    ;; Give the connection time to establish
    (sleep-for 1))
  
  (let ((commands (split-string body "\n" t))
        (results ""))
    (dolist (cmd commands)
      (mcp-inspector-proxy-send-command cmd)
      ;; Give the command time to complete
      (sleep-for 0.5))
    
    ;; Return the contents of the buffer (simplified approach)
    (with-current-buffer (mcp-inspector-proxy-get-buffer)
      (buffer-substring-no-properties (point-min) (point-max)))))

;; Register the org-babel language
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mcp-proxy . t))))

(provide 'mcp-inspector-proxy)
;;; mcp-inspector-proxy.el ends here
