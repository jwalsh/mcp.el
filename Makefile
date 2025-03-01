.DEFAULT_GOAL := help

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q
PACKAGE_NAME = mcp
VERSION = 0.1.0

# Directories
EXAMPLES_DIR = examples
TEST_DIR = tests
BUILD_DIR = build
DIST_DIR = dist

# Source files
EL_FILES = $(wildcard *.el) $(wildcard $(EXAMPLES_DIR)/*/*.el)
TESTS = $(wildcard $(TEST_DIR)/*.el)
ORG_FILES = servers.org README.org

## help            Show this help message and dependency versions
help:
	@echo "MCP.el - Model Context Protocol for Emacs"
	@echo
	@echo "Usage: make [target]"
	@echo
	@echo "Dependencies:"
	@echo "  Current:"
	@$(EMACS) --version | head -n 1
	@node --version | tr -d '\n' && echo " (node)"
	@npm --version | tr -d '\n' && echo " (npm)"
	@echo
	@echo "  Tested with:"
	@echo "    Emacs 30.0.50 (build 1, aarch64-unknown-linux-gnu, GTK+ Version 3.24.33, cairo version 1.16.0)"
	@echo "    node v20.10.0"
	@echo "    npm 10.2.3"
	@echo
	@echo "Targets:"
	@grep -E '^##' $(MAKEFILE_LIST) | grep -v "grep" | sed -E 's/^## //' | column -t -s ':'

## versions        Show dependency versions in detail
versions:
	@echo "Dependency Versions:"
	@echo "  Emacs:"
	@$(EMACS) --version
	@echo "\n  Node:"
	@node --version
	@echo "  NPM:"
	@npm --version
	@echo "\n  System:"
	@uname -a

## all             Build and test the package
all: build test

## readme          Tangle README.org specifically
readme: init
	$(BATCH) --eval "(require 'org)" --eval '(org-babel-tangle-file "README.org")'

## clean           Clean all build artifacts
clean:
	rm -rf $(BUILD_DIR) $(DIST_DIR) *.elc $(EXAMPLES_DIR)/*/*.elc

## init            Create necessary directories
init:
	mkdir -p $(BUILD_DIR) $(DIST_DIR) $(TEST_DIR)
	npm install -g @modelcontextprotocol/server-filesystem @modelcontextprotocol/server-github @modelcontextprotocol/server-time @modelcontextprotocol/server-memory @modelcontextprotocol/server-fetch @modelcontextprotocol/server-sqlite
	pip install mcp-server-git uvx

## tangle          Tangle all org files
tangle: init
	$(BATCH) --eval "(require 'org)" --eval '(mapc #'\''org-babel-tangle-file (list "servers.org" "README.org"))'

## compile         Byte compile all Emacs Lisp files
compile: init tangle
	$(BATCH) -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(EL_FILES)

## test            Run all tests
test: compile
	$(BATCH) -L . -L $(TEST_DIR) --eval "(setq load-prefer-newer t)" -l ert -l $(TEST_DIR)/test-mcp.el -f ert-run-tests-batch-and-exit

## build           Build the package
build: compile
	mkdir -p $(BUILD_DIR)
	cp $(PACKAGE_NAME).el $(BUILD_DIR)/
	cp -r $(EXAMPLES_DIR) $(BUILD_DIR)/
	cp README.org LICENSE $(BUILD_DIR)/ 2>/dev/null || true

## dist            Create distribution package
dist: build
	mkdir -p $(DIST_DIR)
	tar -czf $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz -C $(BUILD_DIR) .

## deps            Install package dependencies
deps:
	$(BATCH) --eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'jsonrpc)"

## run             Run example server
run: compile
	$(EMACS) -Q -L . -L $(EXAMPLES_DIR)/filesystem \
		-l $(PACKAGE_NAME).el \
		-l $(EXAMPLES_DIR)/filesystem/mcp-fs-connection.el \
		--eval "(mcp-fs-connect)"

## dev             Start development environment
dev: compile
	$(EMACS) -Q -L . -L $(EXAMPLES_DIR) \
		-l $(PACKAGE_NAME).el \
		--eval "(require 'mcp-fs-connection)" \
		--eval "(mcp-fs-connect)"

## lint            Check code style
lint:
	$(BATCH) -l package-lint \
		--eval "(setq package-lint-main-file \"$(PACKAGE_NAME).el\")" \
		-f package-lint-batch-and-exit $(EL_FILES)

## docs            Generate documentation
docs: init
	$(BATCH) -l ox-md \
		--eval "(org-babel-tangle-file \"servers.org\")" \
		--eval "(with-current-buffer (find-file \"servers.org\") (org-md-export-to-markdown))"

## package         Create package for MELPA distribution
package: clean compile test docs dist
	@echo "Package $(PACKAGE_NAME)-$(VERSION) built successfully"
	@echo "Distribution archive: $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz"

## test-init       Create test scaffolding
test-init:
	mkdir -p $(TEST_DIR)
	[ -f $(TEST_DIR)/test-mcp.el ] || \
	echo '(require '\''ert)\n(require '\''mcp)\n\n(ert-deftest test-mcp-version () \n  "Test MCP version."\n  (should (string= *MCP-VERSION* "2024-11-05")))' > $(TEST_DIR)/test-mcp.el

.PHONY: help versions all clean init tangle compile test build dist deps run dev lint docs package test-init

## context    Generate file list for prompting, copy to clipboard
context:
	poetry run files-to-prompt -c . 

## files-to-prompt    Generate file list for prompting, copy to clipboard
files-to-prompt:
	@python3.11 -c "import os, sys; print('\n'.join([f for f in os.listdir('.') if os.path.isfile(f) and not f.startswith('.')]))"
	@echo "Files in current directory copied to clipboard"

## config-dirs    Create necessary directories
config-dirs:
	@mkdir -p dist
	@mkdir -p "$(HOME)/Library/Application Support/Claude"

## config-template Create configuration template from sample if it doesn't exist
claude_desktop_config.template.json:
	@echo "Creating configuration template from sample..."
	@cp dist/claude_desktop_config.sample.json claude_desktop_config.template.json
	@echo "Template created at claude_desktop_config.template.json"

## config-generate Generate configuration file from template with expanded variables
.PHONY: config-generate
config-generate: dist/claude_desktop_config.json

dist/claude_desktop_config.json: claude_desktop_config.template.json config-dirs
	@echo "Generating configuration from template..."
	@if [ -z "$(GITHUB_TOKEN)" ]; then \
		echo "Warning: GITHUB_TOKEN environment variable is not set"; \
	fi
	@echo "Using GITHUB_TOKEN: $(shell echo "$(GITHUB_TOKEN)" | cut -c 1-8)..."
	@sed -e "s#\$$HOME#$(HOME)#g" \
		-e "s#\$$GITHUB_TOKEN#$(GITHUB_TOKEN)#g" \
		claude_desktop_config.template.json > dist/claude_desktop_config.json
	@echo "Generated configuration is available at dist/claude_desktop_config.json"

## config-validate Validate the generated configuration
.PHONY: config-validate
config-validate: dist/claude_desktop_config.json
	@echo "Validating configuration..."
	@./validate-config.sh dist/claude_desktop_config.json || echo "⚠️ Some servers could not be validated, but deployment will continue"
	@echo "Configuration validation completed"

## config-deploy  Deploy configuration to Claude Desktop (with backup)
.PHONY: config-deploy
config-deploy: config-validate config-dirs
	@echo "Deploying configuration to Claude Desktop..."
	@if [ -f "$(HOME)/Library/Application Support/Claude/claude_desktop_config.json" ]; then \
		cp "$(HOME)/Library/Application Support/Claude/claude_desktop_config.json" \
		   "$(HOME)/Library/Application Support/Claude/claude_desktop_config.backup.json"; \
		echo "Created backup of existing configuration"; \
	fi
	@./deploy-mcp-config.sh dist/claude_desktop_config.json
	@echo "Configuration deployed successfully! Restart Claude Desktop for changes to take effect."

## start-servers  Start MCP servers based on configuration
.PHONY: start-servers
start-servers: config-validate
	@echo "Starting MCP servers..."
	@./run-servers.sh dist/claude_desktop_config.json
