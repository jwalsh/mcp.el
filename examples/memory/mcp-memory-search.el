;; Search Nodes

;; [[file:../../servers.org::*Search Nodes][Search Nodes:1]]
;;; search.el --- Memory search -*- lexical-binding: t -*-

;;; Commentary:
;; Search functionality for MCP memory server

;;; Code:

(require 'mcp)

(defun mcp-memory-search (query)
  "Search memory nodes with QUERY."
  (interactive "sSearch query: ")
  (when-let* ((connection (mcp-ensure-server)))
    (mcp-async-call-tool 
     connection 
     "search_nodes"
     (list :query query)
     (lambda (result)
       (with-current-buffer (get-buffer-create "*MCP Memory Search*")
         (erase-buffer)
         (when-let* ((content (plist-get result :content)))
           (dolist (item content)
             (when (string= (plist-get item :type) "text")
               (insert (plist-get item :text) "\n"))))
         (display-buffer (current-buffer))))
     (lambda (code message)
       (message "Error: %s - %s" code message)))))

(provide 'mcp-memory-search)
;;; search.el ends here
;; Search Nodes:1 ends here
