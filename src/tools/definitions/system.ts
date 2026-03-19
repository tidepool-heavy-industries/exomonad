import { Tool } from "@modelcontextprotocol/sdk/types.js";

export const systemTools: Tool[] = [
  {
    name: "get_health",
    description:
      "Get server health status including authentication state, active sessions, and configuration. " +
      "Use this to verify the server is ready before starting research workflows.\n\n" +
      "If authenticated=false and having persistent issues:\n" +
      "Consider running cleanup_data(preserve_library=true) + setup_auth for fresh start with clean browser session.",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
  {
    name: "setup_auth",
    description:
      "Google authentication for NotebookLM access - opens a browser window for manual login to your Google account. " +
      "Returns immediately after opening the browser. You have up to 10 minutes to complete the login. " +
      "Use 'get_health' tool afterwards to verify authentication was saved successfully. " +
      "Use this for first-time authentication or when auto-login credentials are not available. " +
      "For switching accounts or rate-limit workarounds, use 're_auth' tool instead.\n\n" +
      "TROUBLESHOOTING for persistent auth issues:\n" +
      "If setup_auth fails or you encounter browser/session issues:\n" +
      "1. Ask user to close ALL Chrome/Chromium instances\n" +
      "2. Run cleanup_data(confirm=true, preserve_library=true) to clean old data\n" +
      "3. Run setup_auth again for fresh start\n" +
      "This helps resolve conflicts from old browser sessions and installation data.",
    inputSchema: {
      type: "object",
      properties: {
        show_browser: {
          type: "boolean",
          description:
            "Show browser window (simple version). Default: true for setup. " +
            "For advanced control, use browser_options instead.",
        },
        browser_options: {
          type: "object",
          description:
            "Optional browser settings. Control visibility, timeouts, and stealth behavior.",
          properties: {
            show: {
              type: "boolean",
              description: "Show browser window (default: true for setup)",
            },
            headless: {
              type: "boolean",
              description: "Run browser in headless mode (default: false for setup)",
            },
            timeout_ms: {
              type: "number",
              description: "Browser operation timeout in milliseconds (default: 30000)",
            },
          },
        },
      },
    },
  },
  {
    name: "re_auth",
    description:
      "Switch to a different Google account or re-authenticate. " +
      "Use this when:\n" +
      "- NotebookLM rate limit is reached (50 queries/day for free accounts)\n" +
      "- You want to switch to a different Google account\n" +
      "- Authentication is broken and needs a fresh start\n\n" +
      "This will:\n" +
      "1. Close all active browser sessions\n" +
      "2. Delete all saved authentication data (cookies, Chrome profile)\n" +
      "3. Open browser for fresh Google login\n\n" +
      "After completion, use 'get_health' to verify authentication.\n\n" +
      "TROUBLESHOOTING for persistent auth issues:\n" +
      "If re_auth fails repeatedly:\n" +
      "1. Ask user to close ALL Chrome/Chromium instances\n" +
      "2. Run cleanup_data(confirm=false, preserve_library=true) to preview old files\n" +
      "3. Run cleanup_data(confirm=true, preserve_library=true) to clean everything except library\n" +
      "4. Run re_auth again for completely fresh start\n" +
      "This removes old installation data and browser sessions that can cause conflicts.",
    inputSchema: {
      type: "object",
      properties: {
        show_browser: {
          type: "boolean",
          description:
            "Show browser window (simple version). Default: true for re-auth. " +
            "For advanced control, use browser_options instead.",
        },
        browser_options: {
          type: "object",
          description:
            "Optional browser settings. Control visibility, timeouts, and stealth behavior.",
          properties: {
            show: {
              type: "boolean",
              description: "Show browser window (default: true for re-auth)",
            },
            headless: {
              type: "boolean",
              description: "Run browser in headless mode (default: false for re-auth)",
            },
            timeout_ms: {
              type: "number",
              description: "Browser operation timeout in milliseconds (default: 30000)",
            },
          },
        },
      },
    },
  },
  {
    name: "cleanup_data",
    description:
      "ULTRATHINK Deep Cleanup - Scans entire system for ALL NotebookLM MCP data files across 8 categories. Always runs in deep mode, shows categorized preview before deletion.\n\n" +
      "⚠️ CRITICAL: Close ALL Chrome/Chromium instances BEFORE running this tool! Open browsers can prevent cleanup and cause issues.\n\n" +
      "Categories scanned:\n" +
      "1. Legacy Installation (notebooklm-mcp-nodejs) - Old paths with -nodejs suffix\n" +
      "2. Current Installation (notebooklm-mcp) - Active data, browser profiles, library\n" +
      "3. NPM/NPX Cache - Cached installations from npx\n" +
      "4. Claude CLI MCP Logs - MCP server logs from Claude CLI\n" +
      "5. Temporary Backups - Backup directories in system temp\n" +
      "6. Claude Projects Cache - Project-specific cache (optional)\n" +
      "7. Editor Logs (Cursor/VSCode) - MCP logs from code editors (optional)\n" +
      "8. Trash Files - Deleted notebooklm files in system trash (optional)\n\n" +
      "Works cross-platform (Linux, Windows, macOS). Safe by design: shows detailed preview before deletion, requires explicit confirmation.\n\n" +
      "LIBRARY PRESERVATION: Set preserve_library=true to keep your notebook library.json file while cleaning everything else.\n\n" +
      "RECOMMENDED WORKFLOW for fresh start:\n" +
      "1. Ask user to close ALL Chrome/Chromium instances\n" +
      "2. Run cleanup_data(confirm=false, preserve_library=true) to preview\n" +
      "3. Run cleanup_data(confirm=true, preserve_library=true) to execute\n" +
      "4. Run setup_auth or re_auth for fresh browser session\n\n" +
      "Use cases: Clean reinstall, troubleshooting auth issues, removing all traces before uninstall, cleaning old browser sessions and installation data.",
    inputSchema: {
      type: "object",
      properties: {
        confirm: {
          type: "boolean",
          description:
            "Confirmation flag. Tool shows preview first, then user confirms deletion. " +
            "Set to true only after user has reviewed the preview and explicitly confirmed.",
        },
        preserve_library: {
          type: "boolean",
          description:
            "Preserve library.json file during cleanup. Default: false. " +
            "Set to true to keep your notebook library while deleting everything else (browser data, caches, logs).",
          default: false,
        },
      },
      required: ["confirm"],
    },
  },
];
