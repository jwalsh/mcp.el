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

# Build targets
all: build test

readme: init
	$(BATCH) --eval "(require 'org)" --eval '(org-babel-tangle-file "README.org")'

clean:
	rm -rf $(BUILD_DIR) $(DIST_DIR) *.elc $(EXAMPLES_DIR)/*/*.elc

init:
	mkdir -p $(BUILD_DIR) $(DIST_DIR) $(TEST_DIR)

tangle: init
	$(BATCH) --eval "(require 'org)" --eval '(mapc #''org-babel-tangle-file (list "servers.org" "README.org"))'

compile: init tangle
	$(BATCH) -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(EL_FILES)

test: compile
	$(BATCH) -L . -L $(TEST_DIR) --eval "(setq load-prefer-newer t)" -l ert -l $(TEST_DIR)/test-mcp.el -f ert-run-tests-batch-and-exit

build: compile
	mkdir -p $(BUILD_DIR)
	cp $(PACKAGE_NAME).el $(BUILD_DIR)/
	cp -r $(EXAMPLES_DIR) $(BUILD_DIR)/
	cp README.org LICENSE $(BUILD_DIR)/ 2>/dev/null || true

dist: build
	mkdir -p $(DIST_DIR)
	tar -czf $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz -C $(BUILD_DIR) .

package: clean compile test docs dist
	@echo "Package $(PACKAGE_NAME)-$(VERSION) built successfully"
	@echo "Distribution archive: $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz"

.PHONY: all clean init tangle compile test build dist package
