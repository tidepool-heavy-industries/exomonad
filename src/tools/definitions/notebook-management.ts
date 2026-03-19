import { Tool } from "@modelcontextprotocol/sdk/types.js";

export const notebookManagementTools: Tool[] = [
  {
    name: "add_notebook",
    description:
      `PERMISSION REQUIRED — Only when user explicitly asks to add a notebook.

## Conversation Workflow (Mandatory)
When the user says: "I have a NotebookLM with X"

1) Ask URL: "What is the NotebookLM URL?"
2) Ask content: "What knowledge is inside?" (1–2 sentences)
3) Ask topics: "Which topics does it cover?" (3–5)
4) Ask use cases: "When should we consult it?"
5) Propose metadata and confirm:
   - Name: [suggested]
   - Description: [from user]
   - Topics: [list]
   - Use cases: [list]
   "Add it to your library now?"
6) Only after explicit "Yes" → call this tool

## Rules
- Do not add without user permission
- Do not guess metadata — ask concisely
- Confirm summary before calling the tool

## Example
User: "I have a notebook with n8n docs"
You: Ask URL → content → topics → use cases; propose summary
User: "Yes"
You: Call add_notebook

## How to Get a NotebookLM Share Link

Visit https://notebooklm.google/ → Login (free: 100 notebooks, 50 sources each, 500k words, 50 daily queries)
1) Click "+ New" (top right) → Upload sources (docs, knowledge)
2) Click "Share" (top right) → Select "Anyone with the link"
3) Click "Copy link" (bottom left) → Give this link to Claude

(Upgraded: Google AI Pro/Ultra gives 5x higher limits)`,
    inputSchema: {
      type: "object",
      properties: {
        url: {
          type: "string",
          description: "The NotebookLM notebook URL",
        },
        name: {
          type: "string",
          description: "Display name for the notebook (e.g., 'n8n Documentation')",
        },
        description: {
          type: "string",
          description: "What knowledge/content is in this notebook",
        },
        topics: {
          type: "array",
          items: { type: "string" },
          description: "Topics covered in this notebook",
        },
        content_types: {
          type: "array",
          items: { type: "string" },
          description:
            "Types of content (e.g., ['documentation', 'examples', 'best practices'])",
        },
        use_cases: {
          type: "array",
          items: { type: "string" },
          description: "When should Claude use this notebook (e.g., ['Implementing n8n workflows'])",
        },
        tags: {
          type: "array",
          items: { type: "string" },
          description: "Optional tags for organization",
        },
      },
      required: ["url", "name", "description", "topics"],
    },
  },
  {
    name: "list_notebooks",
    description:
      "List all library notebooks with metadata (name, topics, use cases, URL). " +
      "Use this to present options, then ask which notebook to use for the task.",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
  {
    name: "get_notebook",
    description: "Get detailed information about a specific notebook by ID",
    inputSchema: {
      type: "object",
      properties: {
        id: {
          type: "string",
          description: "The notebook ID",
        },
      },
      required: ["id"],
    },
  },
  {
    name: "select_notebook",
    description:
      `Set a notebook as the active default (used when ask_question has no notebook_id).

## When To Use
- User switches context: "Let's work on React now"
- User asks explicitly to activate a notebook
- Obvious task change requires another notebook

## Auto-Switching
- Safe to auto-switch if the context is clear and you announce it:
  "Switching to React notebook for this task..."
- If ambiguous, ask: "Switch to [notebook] for this task?"

## Example
User: "Now let's build the React frontend"
You: "Switching to React notebook..." (call select_notebook)`,
    inputSchema: {
      type: "object",
      properties: {
        id: {
          type: "string",
          description: "The notebook ID to activate",
        },
      },
      required: ["id"],
    },
  },
  {
    name: "update_notebook",
    description:
      `Update notebook metadata based on user intent.

## Pattern
1) Identify target notebook and fields (topics, description, use_cases, tags, url)
2) Propose the exact change back to the user
3) After explicit confirmation, call this tool

## Examples
- User: "React notebook also covers Next.js 14"
  You: "Add 'Next.js 14' to topics for React?"
  User: "Yes" → call update_notebook

- User: "Include error handling in n8n description"
  You: "Update the n8n description to mention error handling?"
  User: "Yes" → call update_notebook

Tip: You may update multiple fields at once if requested.`,
    inputSchema: {
      type: "object",
      properties: {
        id: {
          type: "string",
          description: "The notebook ID to update",
        },
        name: {
          type: "string",
          description: "New display name",
        },
        description: {
          type: "string",
          description: "New description",
        },
        topics: {
          type: "array",
          items: { type: "string" },
          description: "New topics list",
        },
        content_types: {
          type: "array",
          items: { type: "string" },
          description: "New content types",
        },
        use_cases: {
          type: "array",
          items: { type: "string" },
          description: "New use cases",
        },
        tags: {
          type: "array",
          items: { type: "string" },
          description: "New tags",
        },
        url: {
          type: "string",
          description: "New notebook URL",
        },
      },
      required: ["id"],
    },
  },
  {
    name: "remove_notebook",
    description:
      `Dangerous — requires explicit user confirmation.

## Confirmation Workflow
1) User requests removal ("Remove the React notebook")
2) Look up full name to confirm
3) Ask: "Remove '[notebook_name]' from your library? (Does not delete the actual NotebookLM notebook)"
4) Only on explicit "Yes" → call remove_notebook

Never remove without permission or based on assumptions.

Example:
User: "Delete the old React notebook"
You: "Remove 'React Best Practices' from your library?"
User: "Yes" → call remove_notebook`,
    inputSchema: {
      type: "object",
      properties: {
        id: {
          type: "string",
          description: "The notebook ID to remove",
        },
      },
      required: ["id"],
    },
  },
  {
    name: "search_notebooks",
    description:
      "Search library by query (name, description, topics, tags). " +
      "Use to propose relevant notebooks for the task and then ask which to use.",
    inputSchema: {
      type: "object",
      properties: {
        query: {
          type: "string",
          description: "Search query",
        },
      },
      required: ["query"],
    },
  },
  {
    name: "get_library_stats",
    description: "Get statistics about your notebook library (total notebooks, usage, etc.)",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
];
