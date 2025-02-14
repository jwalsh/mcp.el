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
ORG_FILES = servers.org

# Build targets
all: build test

# Clean built files
clean:
	rm -rf $(BUILD_DIR) $(DIST_DIR) *.elc $(EXAMPLES_DIR)/*/*.elc

# Create necessary directories
init:
	mkdir -p $(BUILD_DIR) $(DIST_DIR) $(TEST_DIR)

# Tangle org files
tangle: init
	$(BATCH) --eval "(require 'org)" \
		--eval '(org-babel-tangle-file "servers.org")'

# Byte compile
compile: init tangle
	$(BATCH) -L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_FILES)

# Run tests
test: compile
	$(BATCH) -L . -L $(TEST_DIR) \
		--eval "(setq load-prefer-newer t)" \
		-l ert \
		-l $(TEST_DIR)/test-mcp.el \
		-f ert-run-tests-batch-and-exit

# Build package
build: compile
	mkdir -p $(BUILD_DIR)
	cp $(PACKAGE_NAME).el $(BUILD_DIR)/
	cp -r $(EXAMPLES_DIR) $(BUILD_DIR)/
	cp README.md LICENSE $(BUILD_DIR)/ 2>/dev/null || true

# Create distribution package
dist: build
	mkdir -p $(DIST_DIR)
	tar -czf $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz -C $(BUILD_DIR) .

# Install dependencies
deps:
	$(BATCH) --eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'jsonrpc)"

# Run example
run: compile
	$(EMACS) -Q -L . -L $(EXAMPLES_DIR)/filesystem \
		-l $(PACKAGE_NAME).el \
		-l $(EXAMPLES_DIR)/filesystem/mcp-fs-connection.el \
		--eval "(mcp-fs-connect)"

# Development server for testing
dev: compile
	$(EMACS) -Q -L . -L $(EXAMPLES_DIR) \
		-l $(PACKAGE_NAME).el \
		--eval "(require 'mcp-fs-connection)" \
		--eval "(mcp-fs-connect)"

# Check style
lint:
	$(BATCH) -l package-lint \
		--eval "(setq package-lint-main-file \"$(PACKAGE_NAME).el\")" \
		-f package-lint-batch-and-exit $(EL_FILES)

# Generate documentation
docs: init
	$(BATCH) -l ox-md \
		--eval "(org-babel-tangle-file \"servers.org\")" \
		--eval "(with-current-buffer (find-file \"servers.org\") (org-md-export-to-markdown))"

# Package for MELPA
package: clean compile test docs dist
	@echo "Package $(PACKAGE_NAME)-$(VERSION) built successfully"
	@echo "Distribution archive: $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz"

# Create test scaffolding
test-init:
	mkdir -p $(TEST_DIR)
	[ -f $(TEST_DIR)/test-mcp.el ] || \
	echo '(require '\''ert)\n(require '\''mcp)\n\n(ert-deftest test-mcp-version () \n  "Test MCP version."\n  (should (string= *MCP-VERSION* "2024-11-05")))' > $(TEST_DIR)/test-mcp.el

.PHONY: all clean init tangle compile test build dist deps run dev lint docs package test-init