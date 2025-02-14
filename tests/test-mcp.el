;;; test-mcp.el --- Tests for mcp.el -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for MCP functionality

;;; Code:

(require 'ert)
(require 'mcp)

(ert-deftest test-mcp-version ()
  "Test MCP version."
  (should (string= *MCP-VERSION* "2024-11-05")))

(ert-deftest test-mcp-server-connections ()
  "Test server connections hash table exists."
  (should (hash-table-p mcp-server-connections)))

(ert-deftest test-mcp-ensure-server ()
  "Test server connection validation."
  (should (null (mcp-ensure-server)))  ; Should be null when no connection exists
  (let ((test-conn (make-instance 'mcp-process-connection :name "test")))
    (puthash "filesystem" test-conn mcp-server-connections)
    (should (eq test-conn (mcp-ensure-server)))
    (remhash "filesystem" mcp-server-connections)))

(provide 'test-mcp)
;;; test-mcp.el ends here