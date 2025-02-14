# MCP.el - Model Context Protocol for Emacs

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

`MCP.el` provides a Model Context Protocol implementation for Emacs, enabling structured communication between Emacs and external model context servers.

## Features

- Structured communication with MCP servers
- Support for filesystem and generic MCP servers
- Extensible tool and prompt system
- Asynchronous and synchronous operations
- Resource management capabilities
- Comprehensive test suite
- Integration with popular Emacs packages (e.g., gptel)

## Installation

### Requirements

Before installation, ensure you have:
- Emacs 30.0 or later
- Node.js and npm (for MCP servers)

### MELPA (Coming Soon)

MCP.el will be available through MELPA. Once available:

```elisp
M-x package-install RET mcp RET
```

### Manual Installation

To install manually:

1. Clone the repository:
   ```bash
   git clone https://github.com/jwalsh/mcp.el.git
   cd mcp.el
   ```

2. Build and install:
   ```bash
   make
   make install
   ```

## Usage

Basic usage example:

```elisp
(require 'mcp)

;; Initialize a filesystem MCP server connection
(mcp-connect-server "filesystem" 
                    "npx" 
                    '("-y" "@modelcontextprotocol/server-filesystem" "~/Documents/")
                    :initial-callback
                    (lambda (connection)
                      (message "Connected to %s" (jsonrpc-name connection)))
                    :tools-callback
                    (lambda (connection tools)
                      (message "Available tools: %s" tools)))

;; Create a file writing tool
(mcp-make-text-gptel-tool "filesystem" "write_file")

;; Use the tool synchronously
(let ((connection (gethash "filesystem" mcp-server-connections)))
  (mcp-call-tool "write_file" 
                 '(:path "example.txt" 
                   :content "Hello, MCP!")))
```

For more examples, see the `examples/` directory.

## Development

### Prerequisites

- Emacs 30.0 or later
- Node.js and npm (for MCP servers)
- GNU Make
- Required packages:
  - `jsonrpc` for MCP communication
- Development packages:
  - `package-lint` for package validation
  - `gptel` for LLM integration

### Build System

The project uses a standardized Makefile with the following main targets:

- `make all` - Run tests and build the package
- `make test` - Run the test suite
- `make build` - Byte-compile source files
- `make dist` - Create a distribution package
- `make clean` - Remove generated files
- `make lint` - Check code style
- `make docs` - Generate documentation
- `make package` - Create MELPA-ready package

For full details of the build system, see [docs/RFC-001-makefile-standards.md](docs/RFC-001-makefile-standards.md).

### Directory Structure

```
.
├── mcp.el              # Main source file
├── examples/           # Usage examples
│   └── filesystem/     # Filesystem-based implementation
├── test/               # Test files
├── docs/              # Documentation
├── build/             # Build artifacts (generated)
└── dist/              # Distribution packages (generated)
```

### Running Tests

```bash
make test
```

To create new tests:

```bash
make test-init
```

### Development Environment

To start a development environment:

```bash
make dev
```

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Run tests (`make test`)
4. Run style checks (`make lint`)
5. Commit your changes (`git commit -am 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Create a Pull Request

## Documentation

- [User Guide](docs/user-guide.md)
- [API Reference](docs/api-reference.md)
- [Build System Standards](docs/RFC-001-makefile-standards.md)

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Contact

Jason Walsh - [@jwalsh](https://github.com/jwalsh)

Project Link: [https://github.com/jwalsh/mcp.el](https://github.com/jwalsh/mcp.el)