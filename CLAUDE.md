# MCP.el Development Guide

## Build/Test Commands
- Build: `make build`
- Test all: `make test`
- Single test: `$(EMACS) --batch -Q -L . -L tests --eval "(setq load-prefer-newer t)" -l ert -l tests/test-mcp.el -f ert-run-tests-batch "test-name-here"`
- Compile: `make compile`
- Lint: `make lint`
- Run example server: `make run`
- Development environment: `make dev`

## Code Style
- Use lexical binding: Add `;;; filename.el --- Description -*- lexical-binding: t; -*-` to file headers
- Two-space indentation for Emacs Lisp
- Functions use kebab-case with prefix `mcp-` (for public API) or `mcp--` (for internal functions)
- Variables use kebab-case (globals with `*var*` for constants)
- Documentation strings for all public functions and variables
- Error handling through Emacs' standard condition system (see `mcp--process-filter` as example)
- Prefer asynchronous operations with callback functions for network operations
- Follow package.el guidelines for dependencies

## Project Structure
- Core functionality in `mcp.el` 
- Server implementations in `examples/` directory
- Tests in `tests/` directory
- Documentation in `docs/` directory