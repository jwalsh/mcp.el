;; mcp-inspector.el - Emacs integration for MCP Inspector

;;; Code:
(require 'json)
(require 'url)
(require 'org)

(defgroup mcp-inspector nil
  "MCP Inspector integration for Emacs."
  :group 'tools
  :prefix "mcp-inspector-")

(defcustom mcp-inspector-host "localhost"
  "Host for the MCP Inspector server."
  :type 'string
  :group 'mcp-inspector)

(defcustom mcp-inspector-port 8080
  "Port for the MCP Inspector server."
  :type 'integer
  :group 'mcp-inspector)

(defcustom mcp-inspector-api-key nil
  "API key for accessing the MCP Inspector server."
  :type 'string
  :group 'mcp-inspector)

(defcustom mcp-inspector-output-buffer "*MCP Inspector*"
  "Buffer name for MCP Inspector output."
  :type 'string
  :group 'mcp-inspector)

(defvar mcp-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m e") 'mcp-inspector-execute-command)
    (define-key map (kbd "C-c m c") 'mcp-inspector-connect)
    (define-key map (kbd "C-c m d") 'mcp-inspector-disconnect)
    (define-key map (kbd "C-c m q") 'mcp-inspector-generate-qrcode)
    (define-key map (kbd "C-c m h") 'mcp-inspector-history)
    map)
  "Keymap for MCP Inspector mode.")

(define-minor-mode mcp-inspector-mode
  "Minor mode for interacting with MCP Inspector."
  :init-value nil
  :lighter " MCP"
  :keymap mcp-inspector-mode-map
  :group 'mcp-inspector)

;; Core functionality
(defun mcp-inspector-url (endpoint)
  "Create a URL for the given ENDPOINT."
  (format "http://%s:%d/api/%s" 
          mcp-inspector-host 
          mcp-inspector-port 
          endpoint))

(defun mcp-inspector-api-headers ()
  "Return the HTTP headers for API requests."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" mcp-inspector-api-key))))

(defun mcp-inspector-send-request (endpoint method payload callback)
  "Send a request to the MCP Inspector server.
ENDPOINT is the API endpoint.
METHOD is the HTTP method.
PAYLOAD is the request data.
CALLBACK is a function to process the response."
  (let ((url-request-method method)
        (url-request-extra-headers (mcp-inspector-api-headers))
        (url-request-data (when payload
                            (encode-coding-string 
                             (json-encode payload) 'utf-8))))
    (url-retrieve (mcp-inspector-url endpoint)
                  (lambda (status)
                    (goto-char (point-min))
                    (re-search-forward "^$" nil t)
                    (let ((response (buffer-substring (1+ (point)) (point-max))))
                      (funcall callback (json-read-from-string response))))
                  nil t t)))

(defun mcp-inspector-get-buffer ()
  "Get or create the MCP Inspector buffer."
  (get-buffer-create mcp-inspector-output-buffer))

;; Interactive commands
(defun mcp-inspector-connect ()
  "Connect to an MCP server."
  (interactive)
  (let* ((transport (completing-read "Transport Type: " '("STDIO" "TCP" "HTTP") nil t))
         (command (read-string "Command: " "node"))
         (args (read-string "Arguments: " "build/direct-server.js"))
         (payload `((transport . ,transport) 
                    (command . ,command) 
                    (arguments . ,args))))
    (mcp-inspector-send-request "connections" "POST" payload
                               (lambda (response)
                                 (with-current-buffer (mcp-inspector-get-buffer)
                                   (erase-buffer)
                                   (insert "MCP Inspector Connection\n\n")
                                   (insert (format "Status: %s\n" (alist-get 'status response)))
                                   (insert (format "Connection ID: %s\n" (alist-get 'id response)))
                                   (display-buffer (current-buffer)))))))

(defun mcp-inspector-execute-command ()
  "Execute a command on the connected MCP server."
  (interactive)
  (let* ((command (read-string "MCP Command: "))
         (payload `((command . ,command))))
    (mcp-inspector-send-request "execute" "POST" payload
                               (lambda (response)
                                 (with-current-buffer (mcp-inspector-get-buffer)
                                   (goto-char (point-max))
                                   (insert "\n\nCommand Execution:\n")
                                   (insert (format "> %s\n" command))
                                   (insert (format "Result: %s\n" (alist-get 'result response)))
                                   (display-buffer (current-buffer)))))))

(defun mcp-inspector-generate-qrcode (content)
  "Generate a QR code with the given CONTENT."
  (interactive "sQR Code content: ")
  (let ((payload `((content . ,content)
                   (size . 5)
                   (errorCorrectionLevel . "M"))))
    (mcp-inspector-send-request "qrcode" "POST" payload
                               (lambda (response)
                                 (with-current-buffer (mcp-inspector-get-buffer)
                                   (goto-char (point-max))
                                   (insert "\n\nQR Code Generated:\n")
                                   (let ((qr-data (alist-get 'data response)))
                                     ;; Insert QR code as text art
                                     (insert qr-data)
                                     ;; Also save as file if requested
                                     (when (y-or-n-p "Save QR code to file? ")
                                       (let ((file (read-file-name "Save to: " nil nil nil "qrcode.png")))
                                         (with-temp-file file
                                           (insert qr-data)))))
                                   (display-buffer (current-buffer)))))))

(defun mcp-inspector-history ()
  "View command history from the MCP server."
  (interactive)
  (mcp-inspector-send-request "history" "GET" nil
                             (lambda (response)
                               (with-current-buffer (mcp-inspector-get-buffer)
                                 (erase-buffer)
                                 (insert "MCP Command History\n\n")
                                 (let ((entries (alist-get 'entries response)))
                                   (dolist (entry entries)
                                     (insert (format "%s: %s\n" 
                                                    (alist-get 'timestamp entry) 
                                                    (alist-get 'command entry)))))
                                 (display-buffer (current-buffer))))))

(defun mcp-inspector-disconnect ()
  "Disconnect from the MCP server."
  (interactive)
  (mcp-inspector-send-request "connections/current" "DELETE" nil
                             (lambda (response)
                               (with-current-buffer (mcp-inspector-get-buffer)
                                 (goto-char (point-max))
                                 (insert "\n\nDisconnected from MCP server\n")
                                 (display-buffer (current-buffer))))))

;; Org-mode integration
(defun mcp-inspector-org-babel-execute (body params)
  "Execute MCP commands from org-babel.
BODY contains the commands to execute.
PARAMS are the babel execution parameters."
  (let* ((commands (split-string body "\n" t))
         (results '()))
    (dolist (cmd commands)
      (let ((payload `((command . ,cmd))))
        (mcp-inspector-send-request 
         "execute" "POST" payload
         (lambda (response)
           (push (alist-get 'result response) results)))))
    (mapconcat 'identity (reverse results) "\n")))

;; Register the org-babel language
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mcp . t))))

(provide 'mcp-inspector)
;;; mcp-inspector.el ends here
