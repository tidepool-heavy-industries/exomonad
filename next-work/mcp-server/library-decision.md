# MCP Server Library Decision

## Executive Summary

**Recommendation:** Use the **`mcp-server`** Haskell package (v0.1.0.15)

This package provides exactly what we need: type-safe tool definitions, stdio transport, and Template Haskell derivation matching our existing patterns. It's actively maintained (latest release 2025) and well-documented.

## Libraries Evaluated

### Haskell Options

#### 1. mcp-server ⭐ **RECOMMENDED**

**Package:** [`mcp-server`](https://hackage.haskell.org/package/mcp-server) v0.1.0.15
**Maintainer:** drshade (active as of 2025)

**Pros:**
- ✅ **Full MCP 2025-06-18 specification support** - Latest protocol version
- ✅ **Type-safe tool definitions** - Tools defined as Haskell sum types
- ✅ **Template Haskell derivation** - Automatic schema generation (matches our existing pattern)
- ✅ **Stdio transport built-in** - No additional transport layer needed
- ✅ **Minimal boilerplate** - One `deriveToolHandler` call per tool type
- ✅ **Active maintenance** - Updated for 2025, modern GHC support (9.12.2 compatible)
- ✅ **Good documentation** - Clear examples, API docs on Hackage

**Cons:**
- ⚠️ Relatively new package (< 1.0) - but actively maintained
- ⚠️ Template Haskell dependency - but we already use TH extensively

**API Example:**
```haskell
data ScoutTool = MapInfluence { query :: Text } | FindType { typename :: Text }

handleScout :: ScoutTool -> IO Content
handleScout (MapInfluence q) = executeLLMNode scoutNode q
handleScout (FindType t) = executeLLMNode findNode t

main = runMcpServerStdio serverInfo handlers
  where
    handlers = McpServerHandlers
      { tools = Just $(deriveToolHandler ''ScoutTool 'handleScout)
      }
```

**Dependencies:** Reasonable (aeson, base, text, bytestring, TH) - all already in our stack

---

#### 2. mcp (Tritlo)

**Package:** [`mcp`](https://hackage.haskell.org/package/mcp) v0.2.0.1
**Maintainer:** Matthias Pall Gissurarson (Tritlo)

**Pros:**
- ✅ Complete MCP implementation with server/client
- ✅ Dual transport: Stdio + HTTP (with OAuth)
- ✅ MCP 2025-03-26 specification
- ✅ MIT licensed

**Cons:**
- ⚠️ More complex - includes HTTP/OAuth we don't need
- ⚠️ Slightly older protocol version (2025-03-26 vs 2025-06-18)
- ⚠️ No visible Template Haskell derivation - would need manual tool definition
- ⚠️ Heavier dependencies (servant, wai, warp, jose for auth)

**Verdict:** Overkill for our use case. We only need stdio transport.

---

#### 3. json-rpc-server

**Package:** [`json-rpc-server`](https://hackage.haskell.org/package/json-rpc-server) v0.2.6.0
**Last updated:** 2020

**Pros:**
- ✅ Generic JSON-RPC 2.0 server
- ✅ Transport-agnostic (ByteString in/out)
- ✅ Lightweight dependencies

**Cons:**
- ❌ **No MCP layer** - would need to build MCP protocol on top
- ❌ **No type-safe tool definitions** - manual schema writing
- ❌ **Inactive** - Last update 2020, tested only up to GHC 8.10
- ❌ **More boilerplate** - need to manually wire everything

**Verdict:** Too low-level. MCP is more than just JSON-RPC transport.

---

### Rust Options

#### 4. mcp-protocol-sdk (Rust)

**Package:** [`mcp-protocol-sdk`](https://crates.io/crates/mcp-protocol-sdk)
**Description:** Production-ready Rust SDK for MCP

**Pros:**
- ✅ Production-ready, multiple transport support
- ✅ Type-safe (Rust's strong type system)
- ✅ Active ecosystem (multiple Rust MCP crates)

**Cons:**
- ❌ **FFI complexity** - Rust ↔ Haskell FFI is non-trivial
- ❌ **Build complexity** - Need cargo in Haskell build
- ❌ **Unnecessary** - Good Haskell options exist
- ❌ **Lost type safety** - JSON boundaries between languages

**Verdict:** Not justified. Haskell has native solutions.

---

#### 5. pmcp (Rust)

**Package:** [`pmcp`](https://crates.io/crates/pmcp)
**Claim:** "16x faster than TypeScript SDK"

**Pros:**
- ✅ Performance-focused
- ✅ TypeScript SDK compatibility

**Cons:**
- ❌ Same FFI issues as above
- ❌ Performance not relevant for our use case (stdio, not high-throughput)

**Verdict:** Performance irrelevant. Stick with Haskell.

---

## Comparison Matrix

| Criterion | mcp-server | mcp | json-rpc-server | Rust SDK |
|-----------|------------|-----|----------------|----------|
| **Type-safe tools** | ✅ TH derivation | ⚠️ Manual | ❌ None | ⚠️ FFI boundary |
| **Minimal boilerplate** | ✅ | ⚠️ | ❌ | ❌ |
| **Stdio transport** | ✅ Built-in | ✅ Built-in | ⚠️ Manual | ⚠️ FFI |
| **MCP protocol** | ✅ 2025-06-18 | ✅ 2025-03-26 | ❌ None | ✅ |
| **Active maintenance** | ✅ 2025 | ✅ 2025 | ❌ 2020 | ✅ |
| **Build integration** | ✅ | ✅ | ✅ | ❌ cargo |
| **Dependencies** | Light | Heavy | Light | N/A |

---

## Decision Rationale

**`mcp-server` wins because:**

1. **Matches existing patterns** - Template Haskell derivation is already used for:
   - ToolDef typeclass (tool schema generation)
   - TypedTemplate (compile-time Jinja validation)
   - DecisionTools (sum type → MCP tools)

2. **Type safety end-to-end** - From LLMNode definition → MCP schema → tool dispatch, everything stays in Haskell types

3. **Minimal integration effort** - No FFI, no transport layer, no manual schema writing

4. **Latest protocol** - MCP 2025-06-18 spec compliance

5. **Right level of abstraction** - Not too low (json-rpc-server) or too high (mcp with HTTP/OAuth)

---

## Integration Plan

### Existing Code to Leverage

We already have the pieces needed for LLM execution:

```haskell
-- From haskell/effects/llm-interpreter/
executeLLMNode :: LLMNode annotations -> Input -> IO Output

-- From haskell/dsl/core/ (existing ToolDef pattern)
class ToolDef t where
  toolName :: Text
  toolDescription :: Text
  toolSchema :: Value
```

### Proposed Architecture

```haskell
-- New package: haskell/effects/mcp-server/

-- Define tools as sum type (one per LLMNode we want to expose)
data SemanticScoutTools
  = MapInfluence { query :: Text }
  | FindType { typename :: Text }
  deriving (Generic)

-- Handler routes to LLM execution
handleScoutTools :: SemanticScoutTools -> IO Content
handleScoutTools (MapInfluence q) = do
  result <- executeLLMNode mapInfluenceNode q
  pure $ ContentText (renderResult result)

-- Server bootstrap (stdio)
main :: IO ()
main = runMcpServerStdio serverInfo handlers
  where
    handlers = McpServerHandlers
      { tools = Just $(deriveToolHandler ''SemanticScoutTools 'handleScoutTools)
      , resources = Nothing  -- Not needed initially
      , prompts = Nothing    -- Not needed initially
      }
```

### Why This Works

- **LLMNode definitions** remain unchanged (they're the source of truth)
- **Sum type bridges** LLMNode → MCP tool (similar to DecisionTools pattern)
- **Template Haskell** derives the schema automatically
- **Handler function** dispatches to existing `executeLLMNode` infrastructure

---

## Sources

- [mcp-server on Hackage](https://hackage.haskell.org/package/mcp-server)
- [mcp (Tritlo) on Hackage](https://hackage.haskell.org/package/mcp)
- [json-rpc-server on Hackage](https://hackage.haskell.org/package/json-rpc-server)
- [haskell-mcp-server GitHub](https://github.com/drshade/haskell-mcp-server)
- [mcp-protocol-sdk on crates.io](https://crates.io/crates/mcp-protocol-sdk)
- [MCP Specification](https://modelcontextprotocol.io/specification/2025-11-25/basic)
