(defun mcp-run-integration-test ()
  "Run a comprehensive integration test of MCP tools."
  (interactive)
  (let ((test-dir "/tmp/mcp-test")
        (test-repo "test-mcp-integration")
        (start-time (current-time))
        (results '()))
    
    (with-current-buffer (get-buffer-create "*MCP Integration Test*")
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: MCP Tools Integration Test\n")
      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a]") "\n\n")
      (insert "* MCP Tools Integration Test\n\n")
      
      ;; Test sequence - this would need to be adjusted based on actual mcp.el API
      (insert "** Test Sequence\n\n")
      
      ;; 1. Filesystem operations
      (insert "*** Filesystem Operations\n\n")
      (insert "1. Create test directory structure\n")
      (condition-case err
          (progn
            (mcp-call-tool "create_directory" `(:directory_path ,test-dir))
            (mcp-call-tool "write_file" 
                           `(:file_path ,(concat test-dir "/README.md")
                             :content "# MCP Integration Test\n\nThis is a test file."))
            (mcp-call-tool "create_directory" `(:directory_path ,(concat test-dir "/src")))
            (mcp-call-tool "write_file" 
                           `(:file_path ,(concat test-dir "/src/main.py")
                             :content "def main():\n    print('Hello, MCP!')\n\nif __name__ == '__main__':\n    main()"))
            (insert "   ✓ Created directory structure successfully\n"))
        (error
         (insert (format "   ✗ Error creating directory structure: %s\n" 
                         (error-message-string err)))))
      
      (insert "2. Read and verify file contents\n")
      (condition-case err
          (let ((contents (mcp-call-tool "read_file" 
                                        `(:file_path ,(concat test-dir "/src/main.py")))))
            (if (string-match-p "Hello, MCP!" contents)
                (insert "   ✓ File contents verified successfully\n")
              (insert "   ✗ File contents did not match expected output\n")))
        (error
         (insert (format "   ✗ Error reading file: %s\n" 
                         (error-message-string err)))))
      
      ;; 2. Git operations (if a git repo exists or can be created)
      (insert "\n*** Git Operations\n\n")
      (insert "1. Initialize git repository and make initial commit\n")
      (condition-case err
          (progn
            ;; This assumes we're in the test directory and git is available
            ;; In a real test, you'd use mcp.el to change directories and run git commands
            (let ((default-directory test-dir))
              (shell-command "git init")
              (mcp-call-tool "git_add" '(:path "."))
              (mcp-call-tool "git_commit" '(:message "Initial commit"))
              (insert "   ✓ Git repository initialized and initial commit made\n")))
        (error
         (insert (format "   ✗ Error with git initialization: %s\n" 
                         (error-message-string err)))))
      
      ;; 3. GitHub operations (these would typically need actual GitHub credentials)
      (insert "\n*** GitHub Operations (Simulated)\n\n")
      (insert "Note: These operations require GitHub authentication and would affect real repositories.\n")
      (insert "In a real test environment, these should be run against test accounts/repositories.\n\n")
      
      ;; 4. Memory operations
      (insert "\n*** Memory Operations\n\n")
      (insert "1. Create test knowledge graph entities\n")
      (condition-case err
          (progn
            (mcp-call-tool "create_entities" 
                          '(:entities ((:name "Test Entity 1" :type "test")
                                      (:name "Test Entity 2" :type "test"))))
            (mcp-call-tool "create_relations" 
                          '(:relations ((:source "Test Entity 1" 
                                        :relation "is connected to"
                                        :target "Test Entity 2"))))
            (insert "   ✓ Created test knowledge graph entities and relations\n"))
        (error
         (insert (format "   ✗ Error creating knowledge graph entities: %s\n" 
                         (error-message-string err)))))
      
      (insert "2. Query knowledge graph\n")
      (condition-case err
          (let ((results (mcp-call-tool "search_nodes" '(:query "test"))))
            (if (and results (> (length results) 0))
                (insert "   ✓ Successfully queried knowledge graph\n")
              (insert "   ✗ Knowledge graph query returned no results\n")))
        (error
         (insert (format "   ✗ Error querying knowledge graph: %s\n" 
                         (error-message-string err)))))
      
      ;; 5. Sequential thinking
      (insert "\n*** Sequential Thinking\n\n")
      (insert "1. Test sequential thinking tool with simple problem\n")
      (condition-case err
          (let ((result (mcp-call-tool "sequentialthinking" 
                                      '(:thought "What is 2+2?" 
                                        :next_thought_needed t
                                        :thought_number 1
                                        :total_thoughts 2
                                        :is_revision nil
                                        :revises_thought nil
                                        :branch_from_thought nil
                                        :branch_id nil
                                        :needs_more_thoughts nil))))
            ;; Continue with second thought
            (mcp-call-tool "sequentialthinking" 
                          '(:thought "2+2 equals 4" 
                            :next_thought_needed nil
                            :thought_number 2
                            :total_thoughts 2
                            :is_revision nil
                            :revises_thought nil
                            :branch_from_thought nil
                            :branch_id nil
                            :needs_more_thoughts nil))
            (insert "   ✓ Successfully used sequential thinking tool\n"))
        (error
         (insert (format "   ✗ Error using sequential thinking tool: %s\n" 
                         (error-message-string err)))))
      
      ;; 6. Cleanup
      (insert "\n*** Cleanup\n\n")
      (insert "1. Remove test directory and entities\n")
      (condition-case err
          (progn
            ;; Clean up filesystem
            (when (file-exists-p test-dir)
              (delete-directory test-dir t))
            
            ;; Clean up memory
            (mcp-call-tool "delete_entities" 
                          '(:entity_names ["Test Entity 1" "Test Entity 2"]))
            
            (insert "   ✓ Cleanup completed successfully\n"))
        (error
         (insert (format "   ✗ Error during cleanup: %s\n" 
                         (error-message-string err)))))
      
      ;; Test summary
      (let ((elapsed-time (float-time (time-subtract (current-time) start-time))))
        (insert (format "\n** Test Summary\n\n"))
        (insert (format "- Total test time: %.2f seconds\n" elapsed-time))
        (insert "- See above logs for detailed results\n"))
      
      ;; Display the buffer
      (pop-to-buffer (current-buffer)))))
