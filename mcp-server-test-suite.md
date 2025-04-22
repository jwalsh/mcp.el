# MCP Server Test Suite

This comprehensive test suite provides structured tests for all MCP servers configured in your Claude Desktop. Run these tests in sequence to verify each server's functionality.

## Setup

Before running tests:
1. Ensure Claude Desktop is running
2. Run `make config-deploy` to deploy the latest configuration
3. Restart Claude Desktop if needed

## Filesystem Server Tests

```
Testing access to projects directory:
Please list the files in my projects directory.
```

```
Testing access to current repository:
Please list the files in the mcp.el repository.
```

```
Testing file content reading:
Please show me the content of the Makefile in the mcp.el repository.
```

```
Testing file search:
Find all .el files in the mcp.el repository and summarize what each one does.
```

## Git Server Tests

```
Testing branch listing:
What branches exist in the mcp.el repository?
```

```
Testing commit history:
Show me the last 5 commits in the mcp.el repository.
```

```
Testing file status:
What files have changed in the mcp.el working directory?
```

```
Testing git log with path:
Show me the commit history for the Makefile in the mcp.el repository.
```

## GitHub Server Tests

```
Testing repository listing:
List my GitHub repositories.
```

```
Testing repository details:
Show me information about my anthropics/claude-code repository.
```

```
Testing issue listing:
List open issues in the anthropics/claude-code repository.
```

```
Testing file content:
Show me the README from the anthropics/claude-code repository.
```

```
Testing PR listing:
List open pull requests in the anthropics/claude-code repository.
```

## Memory Server Tests

```
Testing basic memory storage:
Remember that I'm Jason Walsh and I'm building MCP.el, a Model Context Protocol integration for Emacs.
```

```
Testing memory recall:
What project am I working on and what is it for?
```

```
Testing memory update:
Remember that MCP.el supports multiple servers including filesystem, git, github, and memory.
```

```
Testing combined recall:
What servers does my MCP.el project support and what's my name?
```

## Fetch Server Tests

```
Testing URL fetching:
Please fetch the content from https://api.github.com/users/jwalsh.
```

```
Testing webpage reading:
Please fetch and summarize the content from https://modelcontextprotocol.ai/
```

```
Testing API integration:
Please fetch weather data for Boston, MA using a weather API.
```

## iTerm MCP Server Tests

```
Testing terminal access:
Please run "ls -la" in my terminal.
```

```
Testing command output:
Please run "ps aux | grep node" and tell me which node processes are running.
```

```
Testing environment variables:
Please check if GITHUB_TOKEN is set in my environment.
```

## Sequential Thinking Server Tests

```
Testing step-by-step reasoning:
Using sequential thinking, explain how you would implement a new MCP server for SQLite database access.
```

```
Testing problem decomposition:
Break down the process of integrating MCP.el with Emacs into sequential steps.
```

```
Testing multi-factor analysis:
Analyze the pros and cons of different approaches to error handling in MCP servers.
```

## Combined Server Tests

```
Testing filesystem and git integration:
Find all git repositories in my projects directory and show me their latest commits.
```

```
Testing memory and github integration:
Remember that I maintain several GitHub repositories. Now fetch and list my main repositories.
```

```
Testing filesystem and memory integration:
Look at the files in my mcp.el repository, then remember the structure of the project to reference later.
```

```
Testing all servers:
Using sequential thinking, analyze my mcp.el repository structure from the filesystem, check its git history, look up similar projects on GitHub, and remember your findings for future reference.
```

## Results Tracking

Track your test results here:

| Server | Test | Status | Notes |
|--------|------|--------|-------|
| Filesystem | Projects access | | |
| Filesystem | Repository access | | |
| Filesystem | File reading | | |
| Filesystem | File search | | |
| Git | Branch listing | | |
| Git | Commit history | | |
| Git | File status | | |
| Git | Path history | | |
| GitHub | Repo listing | | |
| GitHub | Repo details | | |
| GitHub | Issue listing | | |
| GitHub | File content | | |
| GitHub | PR listing | | |
| Memory | Storage | | |
| Memory | Recall | | |
| Memory | Update | | |
| Memory | Combined recall | | |
| Fetch | URL fetching | | |
| Fetch | Webpage reading | | |
| Fetch | API integration | | |
| iTerm | Terminal access | | |
| iTerm | Command output | | |
| iTerm | Environment check | | |
| Sequential | Reasoning | | |
| Sequential | Decomposition | | |
| Sequential | Analysis | | |
| Combined | FS+Git | | |
| Combined | Memory+GitHub | | |
| Combined | FS+Memory | | |
| Combined | All servers | | |