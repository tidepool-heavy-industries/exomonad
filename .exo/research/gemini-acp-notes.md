# Gemini CLI Experimental ACP Research

## 1. ACP (Agent Client Protocol) Overview
The `--experimental-acp` flag puts the Gemini CLI into **Agent Client Protocol** mode. In this mode, it acts as an "Agent" that can be controlled by a "Client" (such as an IDE or another orchestration tool like ExoMonad) using a structured, JSON-RPC based protocol over stdin/stdout.

## 2. Does Gemini CLI need MCP server config (settings.json)?
*   **ACP vs MCP**: ACP does **not** replace MCP (Model Context Protocol). They serve different purposes:
    *   **ACP** is the protocol for the *parent* (Client) to control the *Gemini process* (Agent).
    *   **MCP** is the protocol for the *Gemini process* to access *tools and context* (Servers).
*   **Coexistence**: When running in ACP mode, Gemini CLI **still loads its `settings.json`** to configure the MCP servers (tools) it has access to.
*   **Tunneling**: The ACP protocol specifically supports tunneling MCP messages over ACP. A client can provide its own MCP servers to the Gemini agent using the `_mcp/connect` RPC method. This allows an IDE to expose its internal state as an MCP server that Gemini can query.

## 3. Initial Prompt: ACP vs --prompt-interactive
*   **--prompt-interactive**: Gemini reads the prompt directly from a human user via `stdin`.
*   **--experimental-acp**: Gemini receives its initial (and subsequent) prompts via the `PromptRequest` JSON-RPC message. 
    *   The `connect_and_prompt` function in `rust/exomonad-core/src/services/acp_registry.rs` demonstrates this: it spawns the process, initializes the connection, creates a session, and then calls `conn.prompt(PromptRequest::new(...))` with the initial prompt text.

## 4. Simultaneous Use of MCP and ACP
*   **Yes**, Gemini CLI uses MCP tools and ACP simultaneously.
*   **Implementation**: The `vendor/acp-rust-sdk/src/sacp-proxy/src/mcp_over_acp.rs` provides the necessary message types (`McpOverAcpRequest`, `McpOverAcpNotification`) to facilitate MCP communication within an ACP session.
*   **Role**: In this setup, ACP handles the session lifecycle, while MCP provides the "muscles" (tools) and "eyes" (context).

## 5. Key Protocol Messages
*   `initialize`: Handshake between client and agent.
*   `new_session`: Creates a workspace session (usually bound to a directory).
*   `prompt`: Sends a message to the agent to trigger a response.
*   `session_notification`: Real-time updates from the agent (e.g., text chunks, tool call notifications).
*   `_mcp/*`: Tunneled MCP requests/notifications.

## 6. Project Context
ExoMonad uses `AcpRegistry` to manage these connections, allowing it to spawn Gemini "workers" that it can then interact with programmatically rather than by simulating keypresses in a terminal.
