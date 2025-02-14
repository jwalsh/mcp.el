```
RFC: 001
Title: Standard Makefile Targets for Emacs Lisp Projects
Status: Draft
Author: AYGP System
Created: 2025-02-03
Last-Modified: 2025-02-03
```

# Standard Makefile Targets for Emacs Lisp Projects

## Abstract

This document specifies standard Makefile targets and their implementations for Emacs Lisp projects. It defines a consistent approach for testing, building, distributing, and running Emacs Lisp packages.

## Status of This Document

This document is a draft proposal for standardizing Makefile targets in Emacs Lisp projects.

## Table of Contents

1. Introduction
2. Terminology
3. Standard Targets
4. Implementation Requirements
5. Examples
6. Security Considerations
7. References

## 1. Introduction

Many Emacs Lisp projects use Makefiles for automation, but there's inconsistency in target names and implementations. This RFC proposes a standard set of Makefile targets to promote consistency and interoperability across Emacs Lisp projects.

## 2. Terminology

* ELISP - Emacs Lisp source files
* ELPA - Emacs Lisp Package Archive
* MELPA - Milkypostman's Emacs Lisp Package Archive
* ERT - Emacs Lisp Regression Testing

## 3. Standard Targets

### 3.1. Required Targets

#### `test`
Run all tests for the project.
```makefile
.PHONY: test
test:
        $(EMACS) -Q --batch \
                --eval "(setq load-path (cons \"$(PWD)\" load-path))" \
                --load ert \
                --load test/*.el \
                -f ert-run-tests-batch-and-exit
```

#### `build`
Compile Emacs Lisp files and prepare for distribution.
```makefile
.PHONY: build
build:
        $(EMACS) -Q --batch \
                --eval "(setq load-path (cons \"$(PWD)\" load-path))" \
                -f batch-byte-compile *.el
```

#### `dist`
Create distribution packages.
```makefile
.PHONY: dist
dist: build
        @mkdir -p dist
        @tar czf dist/$(PACKAGE)-$(VERSION).tar.gz \
                --exclude='.git' \
                --exclude='dist' \
                --exclude='test' \
                --exclude='.*' \
                .
```

#### `run`
Load and run the package in Emacs for development.
```makefile
.PHONY: run
run:
        $(EMACS) -Q \
                --eval "(setq load-path (cons \"$(PWD)\" load-path))" \
                --load $(MAIN_FILE)
```

### 3.2. Optional Targets

#### `clean`
Remove generated files.
```makefile
.PHONY: clean
clean:
        rm -rf $(BUILD_DIR) $(DIST_DIR) *.elc $(EXAMPLES_DIR)/*/*.elc
```

#### `lint`
Run linters and static analysis.
```makefile
.PHONY: lint
lint:
        $(EMACS) -Q --batch \
                -l package-lint \
                --eval "(setq package-lint-main-file \"$(PACKAGE_NAME).el\")" \
                -f package-lint-batch-and-exit $(EL_FILES)
```

#### `docs`
Generate documentation from org files.
```makefile
.PHONY: docs
docs: init
        $(EMACS) -Q --batch \
                -l ox-md \
                --eval "(org-babel-tangle-file \"servers.org\")" \
                --eval "(with-current-buffer (find-file \"servers.org\") (org-md-export-to-markdown))"
```

#### `package`
Prepare a complete package for distribution.
```makefile
.PHONY: package
package: clean compile test docs dist
        @echo "Package $(PACKAGE_NAME)-$(VERSION) built successfully"
        @echo "Distribution archive: $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz"
```

#### `deps`
Install required dependencies.
```makefile
.PHONY: deps
deps:
        $(EMACS) -Q --batch \
                --eval "(require 'package)" \
                --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
                --eval "(package-initialize)" \
                --eval "(package-refresh-contents)" \
                --eval "(package-install 'jsonrpc)"
```

#### `dev`
Start a development environment.
```makefile
.PHONY: dev
dev: compile
        $(EMACS) -Q -L . -L $(EXAMPLES_DIR) \
                -l $(PACKAGE_NAME).el \
                --eval "(require 'mcp-fs-connection)" \
                --eval "(mcp-fs-connect)"
```

## 4. Implementation Requirements

### 4.1. Variables

Projects MUST define these core variables at the top of their Makefile:

```makefile
# Core configuration
EMACS ?= emacs
BATCH = $(EMACS) --batch -Q
PACKAGE_NAME = package-name
VERSION = 1.0.0

# Directory structure
EXAMPLES_DIR = examples
TEST_DIR = tests
BUILD_DIR = build
DIST_DIR = dist

# Source files
EL_FILES = $(wildcard *.el) $(wildcard $(EXAMPLES_DIR)/*/*.el)
TESTS = $(wildcard $(TEST_DIR)/*.el)
```

### 4.2. Directory Structure

Projects SHOULD maintain a standard directory structure:

- `examples/` - Example code and usage demonstrations
- `tests/` - Test files and test suites
- `build/` - Temporary build artifacts
- `dist/` - Distribution packages
- `docs/` - Documentation files

### 4.3. Error Handling

- All targets MUST fail immediately if any command fails
- Use of `set -e` or `.SHELLFLAGS := -e` is RECOMMENDED

## 5. Examples

### 5.1. Complete Implementation

```makefile
# Core configuration
EMACS ?= emacs
BATCH = $(EMACS) --batch -Q
PACKAGE_NAME = my-package
VERSION = 1.0.0

# Directories
EXAMPLES_DIR = examples
TEST_DIR = tests
BUILD_DIR = build
DIST_DIR = dist

# Source files
EL_FILES = $(wildcard *.el) $(wildcard $(EXAMPLES_DIR)/*/*.el)
TESTS = $(wildcard $(TEST_DIR)/*.el)
ORG_FILES = documentation.org

# Default target
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
                --eval '(org-babel-tangle-file "$(ORG_FILES)")'

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
                -l $(TEST_DIR)/test-$(PACKAGE_NAME).el \
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
                --eval "(dolist (pkg '(jsonrpc package-lint)) (package-install pkg))"

# Run example
run: compile
        $(EMACS) -Q -L . -L $(EXAMPLES_DIR) \
                -l $(PACKAGE_NAME).el \
                --eval "(require '$(PACKAGE_NAME))"

# Development environment
dev: compile
        $(EMACS) -Q -L . -L $(EXAMPLES_DIR) \
                -l $(PACKAGE_NAME).el

# Check style
lint:
        $(BATCH) -l package-lint \
                --eval "(setq package-lint-main-file \"$(PACKAGE_NAME).el\")" \
                -f package-lint-batch-and-exit $(EL_FILES)

# Generate documentation
docs: init
        $(BATCH) -l ox-md \
                --eval "(org-babel-tangle-file \"$(ORG_FILES)\")" \
                --eval "(with-current-buffer (find-file \"$(ORG_FILES)\") (org-md-export-to-markdown))"

# Package for MELPA
package: clean compile test docs dist
        @echo "Package $(PACKAGE_NAME)-$(VERSION) built successfully"
        @echo "Distribution archive: $(DIST_DIR)/$(PACKAGE_NAME)-$(VERSION).tar.gz"

# Create test scaffolding
test-init:
        mkdir -p $(TEST_DIR)
        [ -f $(TEST_DIR)/test-$(PACKAGE_NAME).el ] || \
        echo '(require '\''ert)\n(require '\''$(PACKAGE_NAME))\n\n(ert-deftest test-$(PACKAGE_NAME)-version () \n  "Test version."\n  (should (string= $(PACKAGE_NAME)-version "$(VERSION)")))' > $(TEST_DIR)/test-$(PACKAGE_NAME).el

.PHONY: all clean init tangle compile test build dist deps run dev lint docs package test-init
```

### 5.2. Minimal Implementation

For small projects, a simplified version may be sufficient:

```makefile
EMACS ?= emacs
PACKAGE_NAME = my-package
VERSION = 1.0.0

.PHONY: all test build clean

all: test build

test:
        $(EMACS) -Q --batch \
                --eval "(setq load-path (cons \"$(PWD)\" load-path))" \
                --load ert \
                --load test/*.el \
                -f ert-run-tests-batch-and-exit

build:
        $(EMACS) -Q --batch \
                --eval "(setq load-path (cons \"$(PWD)\" load-path))" \
                -f batch-byte-compile *.el

clean:
        rm -f *.elc
```

## 6. Security Considerations

### 6.1. Safe Execution

- Never execute arbitrary Elisp code from untrusted sources
- Use `-Q` flag to prevent loading of user's init file
- Verify package signatures when downloading dependencies

### 6.2. File System Safety

- Use absolute paths where possible
- Sanitize file names in tar commands
- Avoid recursive deletes in clean targets

## 7. References

1. GNU Make Manual
2. Emacs Lisp Package Archive Specifications
3. ERT Documentation
4. MELPA Build Process Documentation

## Implementation Notes

Projects implementing this standard SHOULD include this document in their repository at `docs/RFC-001-makefile-standards.md` and reference it in their README.

## Author's Address

AYGP System
https://github.com/jwalsh/mcp.el