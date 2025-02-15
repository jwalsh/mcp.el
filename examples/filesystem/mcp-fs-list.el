;; List Directory

;; [[file:../../servers.org::*List Directory][List Directory:1]]
;;; list.el --- Directory listing -*- lexical-binding: t -*-

;;; Commentary:
;; Directory listing functionality for MCP filesystem

;;; Code:

(require 'mcp)

(defun mcp-fs-list (path)
  "List contents of PATH."
  (interactive "DDirectory: ")
  (when-let* ((connection (mcp-ensure-server)))
    (mcp-async-call-tool 
     connection 
     "list_directory"
     (list :path (expand-file-name path))
     (lambda (result)
       (with-current-buffer (get-buffer-create "*MCP Dir*")
         (erase-buffer)
         (when-let* ((content (plist-get result :content)))
           (dolist (item content)
             (when (string= (plist-get item :type) "text")
               (insert (plist-get item :text) "\n"))))
         (display-buffer (current-buffer))))
     (lambda (code message)
       (message "Error: %s - %s" code message)))))

(provide 'mcp-fs-list)
;;; list.el ends here
;; List Directory:1 ends here
