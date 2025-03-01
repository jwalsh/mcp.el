(defun mcp-test-server-connections ()
  "Test connections to all MCP servers and report results."
  (interactive)
  (let ((servers '("filesystem" "git" "github" "memory" "iterm-mcp" "fetch" "sequential-thinking"))
        (results '()))
    
    (with-current-buffer (get-buffer-create "*MCP Server Connection Test*")
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: MCP Server Connection Test Results\n")
      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a]") "\n\n")
      (insert "* MCP Server Connection Test Results\n\n")
      (insert "| Server | Status | Latency (ms) | Error Message |\n")
      (insert "|--------+--------+--------------+---------------|\n")
      
      (dolist (server servers)
        (let ((start-time (current-time))
              (status nil)
              (latency nil)
              (error-msg ""))
          
          (condition-case err
              (progn
                ;; Try to ping the server - this is a placeholder as the actual
                ;; connection method would depend on how mcp.el implements it
                (cond
                 ((string= server "filesystem")
                  (mcp-call-tool "list_allowed_directories" nil))
                 
                 ((string= server "git")
                  (mcp-call-tool "git_status" nil))
                 
                 ((string= server "github")
                  ;; This is just a simple request that should work with minimal permissions
                  (mcp-call-tool "search_repositories" '(:query "org:emacs-mcp")))
                 
                 ((string= server "memory")
                  (mcp-call-tool "read_graph" nil))
                 
                 ((string= server "iterm-mcp")
                  ;; This could fail if no terminal is active
                  (mcp-call-tool "read_terminal_output" nil))
                 
                 ((string= server "fetch")
                  (mcp-call-tool "fetch" '(:url "https://example.com" :extract_as_markdown t)))
                 
                 ((string= server "sequential-thinking")
                  ;; This is a minimal call to test connection
                  (mcp-call-tool "sequentialthinking" 
                                 '(:thought "Initial thought" 
                                   :next_thought_needed nil
                                   :thought_number 1
                                   :total_thoughts 1
                                   :is_revision nil
                                   :revises_thought nil
                                   :branch_from_thought nil
                                   :branch_id nil
                                   :needs_more_thoughts nil))))
                
                (setq status "Connected")
                (setq latency (float-time (time-subtract (current-time) start-time))))
            
            (error
             (setq status "Failed")
             (setq latency (float-time (time-subtract (current-time) start-time)))
             (setq error-msg (error-message-string err))))
          
          ;; Log the result
          (push (list server status latency error-msg) results)
          
          ;; Output to the buffer
          (insert (format "| %s | %s | %.2f | %s |\n" 
                          server 
                          (if (string= status "Connected") "✓" "✗")
                          (* 1000 latency)
                          error-msg))))
      
      ;; Add a summary
      (let ((connected (length (cl-remove-if-not 
                               (lambda (r) (string= (nth 1 r) "Connected"))
                               results))))
        (insert "\n** Summary\n\n")
        (insert (format "- Total servers: %d\n" (length servers)))
        (insert (format "- Connected servers: %d\n" connected))
        (insert (format "- Failed connections: %d\n" (- (length servers) connected))))
      
      ;; Display the buffer
      (pop-to-buffer (current-buffer)))))
