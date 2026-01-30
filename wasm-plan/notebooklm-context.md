# Extism Integration: Haskell Guest + Rust Host

Context document for NotebookLM oracle. This covers embedding Haskell (compiled to WASM) in a Rust runtime using Extism.

---

## Architecture Overview

We're building a system where:
- **Rust** is the host runtime handling all IO (networking, containers, async, telemetry)
- **Haskell** is compiled to WASM and runs as an Extism plugin
- Haskell contains pure business logic and yields "effects" (requests for IO)
- Rust interprets those effects by executing actual IO operations

```
┌─────────────────────────────────────────────────────────────────┐
│ Rust Runtime (Extism Host)                                      │
│                                                                 │
│  Services: Git, GitHub, Docker, Logging, Telemetry              │
│  HTTP server (axum), async runtime (Tokio)                      │
│                                                                 │
│  Host Functions: git_get_branch, github_create_pr, log_info     │
└──────────────────────────────┬──────────────────────────────────┘
                               │ Extism host function calls
                               │ (JSON over WASM linear memory)
┌──────────────────────────────▼──────────────────────────────────┐
│ Haskell WASM Plugin (Extism Guest)                              │
│                                                                 │
│  Entry points: handle_mcp_call, handle_pre_tool_use, etc.       │
│  Pure logic: role dispatch, permission checks, orchestration    │
│  Uses freer-simple for effect system                            │
└─────────────────────────────────────────────────────────────────┘
```

---

## Extism Basics

### What is Extism?

Extism is a framework for building plugin systems using WebAssembly. It provides:
- **Host SDKs** (Rust, Go, Python, etc.) for loading and calling WASM plugins
- **PDKs** (Plugin Development Kits) for writing plugins in various languages
- **Host functions**: mechanism for plugins to call back into the host for IO
- **Memory management**: utilities for passing data across the WASM boundary

### Key Concepts

- **Plugin**: A WASM module loaded by the host
- **Export**: A function the plugin exposes for the host to call
- **Host Function**: A function the host exposes for the plugin to call
- **Linear Memory**: The WASM memory space where data is passed (as bytes)

---

## GHC WASM Backend

### Reactor vs Command Model

GHC can compile Haskell to WASM in two modes:

**Command Mode (default):**
- Runs `main`, then exits
- Like a CLI program
- NOT what we want

**Reactor Mode (what we need):**
- Exports functions that can be called repeatedly
- Stays alive between calls
- Like a library or service

### Required GHC Linker Flags

```
-no-hs-main                    # Don't generate default main
-optl-mexec-model=reactor      # Use reactor model
-optl-Wl,--export=hs_init      # Export RTS initialization
-optl-Wl,--export=my_function  # Export each function
-optl-Wl,--allow-undefined     # Allow missing symbols (host functions)
```

### Cabal Configuration

```cabal
executable wasm-guest
  main-is: Main.hs
  build-depends: base, extism-pdk, aeson, freer-simple

  if arch(wasm32)
    ghc-options:
      -no-hs-main
      -optl-mexec-model=reactor
      -optl-Wl,--export=hs_init
      -optl-Wl,--export=handle_mcp_call
      -optl-Wl,--export=handle_pre_tool_use
      -optl-Wl,--allow-undefined
```

### RTS Initialization

**Critical**: The Rust host MUST call `hs_init` exactly once after loading the plugin, before calling any other exports.

```rust
// Rust host
plugin.call::<_, ()>("hs_init", "")?;  // First!
plugin.call("handle_mcp_call", input)?;  // Then business logic
```

### Toolchain Installation

Don't use standard ghcup. Use `ghc-wasm-meta`:
- Nix: Add their cache, `nix develop`
- Docker: Use `ghc-wasm-meta:master` image
- Target GHC 9.10+ for best RTS support

### Binary Size

Haskell WASM binaries are large (10MB+). Optimize with:
```bash
wasm-opt -O3 plugin.wasm -o plugin-opt.wasm  # Cuts ~50%
```

---

## Haskell PDK (extism-pdk)

### Key Functions

```haskell
import Extism.PDK
import Extism.PDK.Memory (alloc, load, free, MemoryOffset(..))

-- Read input from host (for plugin entry points)
inputJSON :: FromJSON a => IO a

-- Write output to host (for plugin entry points)
outputJSON :: ToJSON a => a -> IO ()

-- Memory management (for host function calls)
alloc :: ByteString -> IO MemoryOffset   -- Allocate bytes, get pointer
load :: MemoryOffset -> IO (Either String ByteString)  -- Read from pointer
free :: MemoryOffset -> IO ()  -- Free allocated memory
```

### Entry Point Pattern

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Extism.PDK
import Control.Exception (try, SomeException)

-- Export to host
foreign export ccall handle_mcp_call :: IO Int32

handle_mcp_call :: IO Int32
handle_mcp_call = do
  result <- try @SomeException $ do
    input <- inputJSON @McpRequest
    response <- processRequest input
    outputJSON response
  case result of
    Right () -> pure 0   -- Success
    Left err -> do
      outputJSON $ ErrorResponse (show err)
      pure 1  -- Error
```

### Host Function Call Pattern

Host functions are imported as FFI with Word64 (pointers) for data passing:

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word (Word64)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Extism.PDK.Memory (alloc, load, free, MemoryOffset(..))

-- Raw FFI import (Word64 = pointer)
foreign import ccall "git_get_branch"
  raw_git_get_branch :: Word64 -> IO Word64

-- High-level wrapper
callHost :: (ToJSON req, FromJSON resp)
         => (Word64 -> IO Word64) -> req -> IO (Either String resp)
callHost rawFn request = do
  -- 1. Serialize request to JSON
  let reqBytes = toStrict (encode request)

  -- 2. Allocate in WASM memory, get pointer
  reqOffset <- alloc reqBytes

  -- 3. Call host function with pointer
  respOffset <- rawFn (unMemoryOffset reqOffset)

  -- 4. Read response from returned pointer
  respResult <- load (MemoryOffset respOffset)

  -- 5. Free our input allocation (we allocated, we free)
  free reqOffset

  -- 6. Decode response
  case respResult of
    Left err -> pure $ Left $ "Memory read failed: " <> err
    Right bytes -> case decode (fromStrict bytes) of
      Nothing -> pure $ Left "JSON decode failed"
      Just x  -> pure $ Right x

-- Fire-and-forget variant (no return value)
callHostVoid :: ToJSON req => (Word64 -> IO ()) -> req -> IO ()
callHostVoid rawFn request = do
  let reqBytes = toStrict (encode request)
  reqOffset <- alloc reqBytes
  rawFn (unMemoryOffset reqOffset)
  free reqOffset

-- Usage
gitGetBranch :: Text -> IO (Either String Text)
gitGetBranch dir = callHost raw_git_get_branch (GitGetBranchRequest dir)
```

---

## Rust Host SDK (extism crate)

### Loading a Plugin

```rust
use extism::{Plugin, Manifest, Wasm};

let wasm = Wasm::file("./plugin.wasm");
let manifest = Manifest::new([wasm]);
let mut plugin = Plugin::new(&manifest, host_functions, true)?;

// Initialize Haskell RTS (required!)
plugin.call::<_, ()>("hs_init", "")?;

// Call exports
let result: Vec<u8> = plugin.call("handle_mcp_call", input_json)?;
```

### Defining Host Functions

```rust
use extism::{Function, Val, ValType, UserData, CurrentPlugin};

fn define_git_get_branch(services: Arc<Services>) -> Function {
    Function::new(
        "git_get_branch",           // Name (matches Haskell FFI import)
        [ValType::I64],             // Input: pointer to JSON
        [ValType::I64],             // Output: pointer to JSON
        Some(UserData::new(services)),
        |plugin, inputs, outputs, user_data| {
            let services = user_data.get::<Arc<Services>>()?;

            // 1. Read input from WASM memory
            let offset = inputs[0].unwrap_i64() as u64;
            let req_bytes = plugin.memory_read(offset)?;
            let req: GitGetBranchRequest = serde_json::from_slice(&req_bytes)?;

            // 2. Execute actual IO (see async bridging below)
            let handle = tokio::runtime::Handle::current();
            let result = handle.block_on(async {
                services.git.get_branch(&req.dir).await
            });

            // 3. Serialize response
            let resp = match result {
                Ok(branch) => EffectResult::Success(branch),
                Err(e) => EffectResult::Error { message: e.to_string() },
            };
            let resp_bytes = serde_json::to_vec(&resp)?;

            // 4. Allocate in WASM memory for response
            let resp_offset = plugin.memory_alloc(resp_bytes.len() as u64)?;
            plugin.memory_write(resp_offset, &resp_bytes)?;

            // 5. Return pointer
            outputs[0] = Val::I64(resp_offset as i64);
            Ok(())
        },
    )
}

// Fire-and-forget variant (no return)
fn define_log_info(services: Arc<Services>) -> Function {
    Function::new(
        "log_info",
        [ValType::I64],
        [],  // No output
        Some(UserData::new(services)),
        |plugin, inputs, _outputs, user_data| {
            let offset = inputs[0].unwrap_i64() as u64;
            let bytes = plugin.memory_read(offset)?;
            let msg: LogMessage = serde_json::from_slice(&bytes)?;
            tracing::info!("{}", msg.message);
            Ok(())
        },
    )
}
```

### Registering Host Functions

```rust
let host_functions = vec![
    define_git_get_branch(services.clone()),
    define_github_create_pr(services.clone()),
    define_log_info(services.clone()),
];

let plugin = Plugin::new(&manifest, host_functions, true)?;
```

---

## Async Bridging (Critical Pattern)

### The Problem

- Extism `plugin.call()` is **synchronous** (blocking)
- Rust async code runs on Tokio (non-blocking)
- Host functions may need to call async IO (HTTP, database, etc.)

### Solution: spawn_blocking + block_on

```rust
// HTTP handler (async context)
async fn handle_mcp_request(
    plugin: Arc<Mutex<Plugin>>,
    input: String,
) -> Result<String> {
    // Offload blocking plugin call to dedicated thread pool
    tokio::task::spawn_blocking(move || {
        let mut plugin = plugin.lock().unwrap();
        plugin.call("handle_mcp_call", &input)
    }).await?
}

// Inside host function (already in blocking context from spawn_blocking)
fn my_host_function(...) {
    // Bridge back to async for IO
    let handle = tokio::runtime::Handle::current();
    let result = handle.block_on(async {
        reqwest::get("https://api.github.com/...").await
    });
}
```

### Why This Works

```
Tokio async task
    │
    └── spawn_blocking ──→ Blocking thread pool
                               │
                               └── plugin.call (sync, blocks this thread)
                                       │
                                       └── host function called
                                               │
                                               └── block_on(async IO)
                                                       │
                                                       └── Tokio executes IO
                                                               │
                                                       ←───────┘
                                               ←───────┘
                                       ←───────┘
                               ←───────┘
    ←───────────────────────────┘
```

---

## Memory Ownership Rules

| Scenario | Who Allocates | Who Frees |
|----------|---------------|-----------|
| Plugin input (host calls plugin) | Host | Host (Extism manages) |
| Plugin output (plugin returns to host) | Plugin | Host (Extism manages) |
| Host function input (plugin calls host) | Plugin | Plugin |
| Host function output (host returns to plugin) | Host | Extism (on reset) |

**Rule of thumb**: Caller allocates input, caller frees input.

---

## JSON Serialization Contract

### Adjacently Tagged Sum Types

All sum types use this format:

```json
{
  "kind": "VariantName",
  "payload": { ... }
}
```

### Rust Configuration

```rust
#[derive(Serialize, Deserialize)]
#[serde(tag = "kind", content = "payload")]
enum Effect {
    GitGetBranch { dir: String },
    Log { level: String, message: String },
}
```

### Haskell Configuration

```haskell
import Data.Aeson

data Effect
  = GitGetBranch { dir :: Text }
  | Log { level :: Text, message :: Text }
  deriving (Generic)

instance ToJSON Effect where
  toJSON = genericToJSON defaultOptions
    { sumEncoding = TaggedObject "kind" "payload" }

instance FromJSON Effect where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TaggedObject "kind" "payload" }
```

### Result Type

```json
// Success
{"kind": "Success", "payload": "main"}

// Error
{"kind": "Error", "payload": {"message": "Branch not found"}}
```

---

## Error Handling

### Haskell: Catch at Export Boundary

```haskell
handle_mcp_call :: IO Int32
handle_mcp_call = do
  result <- try @SomeException $ do
    input <- inputJSON
    response <- businessLogic input
    outputJSON response
  case result of
    Right () -> pure 0
    Left err -> do
      outputJSON $ ErrorResponse (pack $ show err)
      pure 1
```

### Rust: Check Return Code

```rust
// If using call_with_return_code
let (code, output) = plugin.call_with_return_code("handler", input)?;
if code != 0 {
    let error: ErrorResponse = serde_json::from_slice(&output)?;
    return Err(error.into());
}
```

### Panic Handling

- Haskell panics are caught by Extism and surfaced as plugin errors
- Stack traces inside WASM are often opaque
- Always use try/catch at export boundaries for clean error messages

---

## Hot Reload

```rust
impl PluginManager {
    pub async fn reload(&self) -> Result<()> {
        let wasm = Wasm::file(&self.wasm_path);
        let manifest = Manifest::new([wasm]);
        let new_plugin = Plugin::new(&manifest, self.host_functions.clone(), true)?;

        // Initialize RTS
        new_plugin.call::<_, ()>("hs_init", "")?;

        // Swap
        let mut guard = self.plugin.write().await;
        *guard = new_plugin;
        Ok(())
    }
}
```

---

## Common Gotchas

1. **Forgot `hs_init`**: Plugin crashes on first call
2. **Called plugin.call from async**: Blocks Tokio reactor, deadlock
3. **Wrong sum type encoding**: JSON parse fails silently
4. **Memory leak**: Forgot to free allocations in callHost
5. **Missing `--allow-undefined`**: Link fails for host function imports
6. **Command instead of reactor**: Plugin runs main and exits immediately

---

## Quick Reference

### Haskell Plugin Entry Point
```haskell
foreign export ccall my_handler :: IO Int32
my_handler = do { inputJSON >>= process >>= outputJSON; pure 0 }
```

### Haskell Host Function Call
```haskell
foreign import ccall "host_fn" raw :: Word64 -> IO Word64
result <- callHost raw request  -- See callHost pattern above
```

### Rust Load Plugin
```rust
let plugin = Plugin::new(&manifest, host_fns, true)?;
plugin.call::<_, ()>("hs_init", "")?;
```

### Rust Host Function
```rust
Function::new("name", [I64], [I64], data, |plugin, ins, outs, _| { ... })
```

### Rust Call Plugin
```rust
spawn_blocking(|| plugin.call("handler", input)).await?
```

### GHC Flags
```
-no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=X -optl-Wl,--allow-undefined
```
