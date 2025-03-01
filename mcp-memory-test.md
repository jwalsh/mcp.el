# MCP Memory Server Test

The memory server allows Claude to remember information across sessions. This file contains tests to verify the memory server is working correctly.

## Basic Memory Tests

1. **Remember a fact**:
   "Remember that my favorite programming language is Emacs Lisp."

2. **Recall the fact**:
   "What is my favorite programming language?"

3. **Remember a project**:
   "Remember that I'm working on a project called MCP.el that connects Claude to Emacs."

4. **Recall the project**:
   "What project am I working on?"

5. **Remember details**:
   "Remember that MCP.el uses JSONRPC for communication between Emacs and MCP servers."

6. **Recall combined information**:
   "What can you tell me about the MCP.el project and how it relates to my favorite programming language?"

## Advanced Memory Tests

7. **Store structured information**:
   "Remember that MCP.el has the following components: a core library, protocol adapters, and example servers."

8. **Reference memory when answering**:
   "How could I extend MCP.el to support a new type of server?"

9. **Memory correction**:
   "Actually, my favorite programming language is Common Lisp, not Emacs Lisp."

10. **Verify correction**:
    "What is my favorite programming language now?"

11. **Remember a process**:
    "Remember that to deploy MCP.el I need to run 'make config-deploy' and then restart Claude Desktop."

12. **Test with filesystem context**:
    "Based on what you remember about my project and what you can see in the filesystem, how does MCP.el handle server connections?"

## Tips for Testing

- Run these tests in sequence to verify memory persistence
- Wait a few minutes between some tests to ensure long-term storage
- Try closing and reopening Claude Desktop to test persistence across sessions
- Combine memory tests with other MCP servers to test integration