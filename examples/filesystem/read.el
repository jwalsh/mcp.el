;; Read File

;; [[file:../../servers.org::*Read File][Read File:1]]
;;; read.el --- File reading -*- lexical-binding: t -*-

;;; Commentary:
;; File reading functionality for MCP filesystem

;;; Code:

(require 'mcp)

(defun mcp-fs-read (path)
  "Read contents of file at PATH."
  (interactive "fFile: ")
  (when-let* ((connection (mcp-ensure-server)))
    (mcp-async-call-tool 
     connection 
     "read_file"
     (list :path (expand-file-name path))
     (lambda (result)
       (with-current-buffer (get-buffer-create "*MCP File*")
         (erase-buffer)
         (when-let* ((content (plist-get result :content)))
           (dolist (item content)
             (when (string= (plist-get item :type) "text")
               (insert (plist-get item :text) "\n"))))
         (display-buffer (current-buffer))))
     (lambda (code message)
       (message "Error: %s - %s" code message)))))

(provide 'mcp-fs-read)
;;; read.el ends here
;; Read File:1 ends here
