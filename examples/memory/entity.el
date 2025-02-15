;; Create Entity

;; [[file:../../servers.org::*Create Entity][Create Entity:1]]
;;; entity.el --- Memory entity management -*- lexical-binding: t -*-

;;; Commentary:
;; Entity management for MCP memory server

;;; Code:

(require 'mcp)

(defun mcp-memory-create-entity (name type observations)
  "Create entity with NAME, TYPE and OBSERVATIONS."
  (interactive "sEntity name: \nsEntity type: \nsObservations (comma-separated): ")
  (when-let* ((connection (mcp-ensure-server)))
    (mcp-async-call-tool 
     connection 
     "create_entities"
     (list :entities 
           (list (list :name name
                      :entityType type
                      :observations (split-string observations ","))))
     (lambda (result)
       (message "Created entity %s" name))
     (lambda (code message)
       (message "Error: %s - %s" code message)))))

(provide 'mcp-memory-entity)
;;; entity.el ends here
;; Create Entity:1 ends here
