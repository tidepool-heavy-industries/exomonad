# Implementation Details (Research Findings)

Concrete patterns and gotchas from research. Reference this when implementing.

## 1. Haskell PDK API

### Key Functions

```haskell
import Extism.PDK
import Extism.PDK.Memory (alloc, load, free, MemoryOffset(..))

-- Read plugin input (from Rust host)
inputJSON :: FromJSON a => IO a

-- Write plugin output (to Rust host)
outputJSON :: ToJSON a => a -> IO ()

-- Memory management for host function calls
alloc :: ByteString -> IO MemoryOffset
load :: MemoryOffset -> IO (Either String ByteString)
free :: MemoryOffset -> IO ()
```

### Host Function Call Pattern

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word (Word64)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Extism.PDK.Memory (alloc, load, free, MemoryOffset(..))

-- Raw import (Word64 pointers only)
foreign import ccall "git_get_branch"
  raw_git_get_branch :: Word64 -> IO Word64

-- High-level wrapper
callHost :: (ToJSON req, FromJSON resp) => (Word64 -> IO Word64) -> req -> IO (Either String resp)
callHost rawFn request = do
  -- Allocate input in WASM memory
  let reqBytes = toStrict (encode request)
  reqOffset <- alloc reqBytes

  -- Call host (passing pointer)
  respOffset <- rawFn (unMemoryOffset reqOffset)

  -- Read output from WASM memory
  respResult <- load (MemoryOffset respOffset)

  -- Free input memory (we allocated it)
  free reqOffset

  -- Decode response
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
```

## 2. GHC WASM Build Flags

### Required Flags (Reactor Mode)

```cabal
executable wasm-guest
  ghc-options:
    -no-hs-main                    -- No default main entry point
    -optl-mexec-model=reactor      -- Reactor, not command
    -optl-Wl,--export=hs_init      -- Export RTS init
    -optl-Wl,--export=handle_mcp_call
    -optl-Wl,--export=handle_pre_tool_use
    -optl-Wl,--allow-undefined     -- Allow missing host functions at link time
```

### RTS Initialization

**Critical**: Rust host MUST call `hs_init` once before any other exports.

```rust
// First call after loading plugin
plugin.call::<_, ()>("hs_init", "")?;

// Then call business logic
plugin.call("handle_mcp_call", input)?;
```

### Installation

Use `ghc-wasm-meta` (not standard ghcup):
- Nix: Add their cache, use `nix develop`
- Docker: `ghc-wasm-meta:master` image
- Target GHC 9.10+ for best RTS support

### Binary Size

Haskell WASM binaries are large (10MB+). Optimize with:
```bash
wasm-opt -O3 plugin.wasm -o plugin-opt.wasm
# Often cuts size by 50%
```

## 3. quicktype Haskell Limitations

quicktype generates valid Haskell but with default aeson encoding, NOT adjacently tagged.

### Solution: Post-Process Instances

```haskell
-- quicktype generates this:
data MyEffect = GitGetBranch GitGetBranchPayload | Log LogPayload
  deriving (Show, Generic)

-- We must REPLACE the instances with:
instance ToJSON MyEffect where
  toJSON = genericToJSON defaultOptions
    { sumEncoding = TaggedObject "kind" "payload" }

instance FromJSON MyEffect where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TaggedObject "kind" "payload" }
```

### Recommended Workflow

1. Generate types with quicktype
2. Keep only the `data` declarations
3. Add our own instances with correct `sumEncoding`
4. Or: use Template Haskell `deriveJSON` with options

### Script to Fix Instances

```bash
# gen-types-haskell target should:
# 1. Run quicktype
# 2. Run a sed/awk script to replace instances
# 3. Or: generate to .Types.hs, manually write instances in separate file
```

## 4. Rust Host Patterns

### Async Bridge (Critical)

Extism `plugin.call` is **synchronous**. Cannot call from async context directly.

```rust
// WRONG - blocks Tokio reactor
async fn handle_request(plugin: &Plugin, input: &str) {
    plugin.call("handler", input)?; // BAD: blocks async thread
}

// CORRECT - offload to blocking thread
async fn handle_request(plugin: Arc<Mutex<Plugin>>, input: String) -> Result<String> {
    tokio::task::spawn_blocking(move || {
        let plugin = plugin.lock().unwrap();
        plugin.call("handler", &input)
    }).await?
}
```

### Host Function with Async IO

Inside host function, bridge back to async:

```rust
fn define_github_list_issues(services: Arc<Services>) -> Function {
    Function::new(
        "github_list_issues",
        [ValType::I64],
        [ValType::I64],
        Some(UserData::new(services)),
        |plugin, inputs, outputs, user_data| {
            let services = user_data.get::<Arc<Services>>()?;

            // Read input
            let offset = inputs[0].unwrap_i64() as u64;
            let req_bytes = plugin.memory_read(offset)?;
            let req: ListIssuesRequest = serde_json::from_slice(&req_bytes)?;

            // Bridge to async (we're in spawn_blocking context)
            let handle = tokio::runtime::Handle::current();
            let result = handle.block_on(async {
                services.github.list_issues(&req.repo, req.filter.as_ref()).await
            });

            // Write output
            let resp = match result {
                Ok(issues) => EffectResult::Success(serde_json::to_value(issues)?),
                Err(e) => EffectResult::Error { message: e.to_string() },
            };
            let resp_bytes = serde_json::to_vec(&resp)?;
            let resp_offset = plugin.memory_alloc(resp_bytes.len() as u64)?;
            plugin.memory_write(resp_offset, &resp_bytes)?;

            outputs[0] = Val::I64(resp_offset as i64);
            Ok(())
        },
    )
}
```

### Plugin Lifecycle

```rust
// Load once
let plugin = Plugin::new(&manifest, host_functions, true)?;

// Initialize RTS (once per load)
plugin.call::<_, ()>("hs_init", "")?;

// Call exports (many times)
let result = plugin.call("handle_mcp_call", input)?;

// Hot reload
let new_plugin = Plugin::new(&manifest, host_functions, true)?;
new_plugin.call::<_, ()>("hs_init", "")?;
// Swap in new_plugin
```

## 5. freer-simple Migration

Existing handlers stay mostly unchanged. Only interpreters change.

### Before (Native IO)

```haskell
runGitHubIO :: GitHubConfig -> Eff (GitHub ': effs) a -> Eff effs a
runGitHubIO config = interpret $ \case
  ListIssues repo filter -> sendM $ do
    -- Actually call GitHub API via HTTP
    response <- httpGet (buildUrl repo filter)
    pure (parseIssues response)
```

### After (WASM Host Calls)

```haskell
runGitHubWasm :: Eff (GitHub ': effs) a -> Eff effs a
runGitHubWasm = interpret $ \case
  ListIssues repo filter -> sendM $ do
    result <- callHost raw_github_list_issues (ListIssuesPayload repo filter)
    case result of
      Left err -> error $ "GitHub effect failed: " <> err
      Right issues -> pure issues
```

### Handler Logic Unchanged

```haskell
-- This stays exactly the same
handleFilePR :: Member GitHub effs => PRSpec -> Eff effs PullRequest
handleFilePR spec = do
  issues <- listIssues spec.repo Nothing
  -- ... logic ...
  createPR spec.repo prSpec
```

## 6. Error Handling

### Haskell Side

Wrap exports in catch to return structured errors:

```haskell
handle_mcp_call :: IO Int32
handle_mcp_call = do
  result <- try @SomeException $ do
    input <- inputJSON @McpCallInput
    response <- runHostEffects $ dispatchMcpCall input
    outputJSON response
  case result of
    Right () -> pure 0
    Left err -> do
      outputJSON $ McpError (T.pack $ show err)
      pure 1
```

### Rust Side

Check return code and read error from output:

```rust
let (code, output) = plugin.call_with_return_code("handle_mcp_call", input)?;
if code != 0 {
    let error: McpError = serde_json::from_slice(&output)?;
    return Err(error.into());
}
let response: McpResponse = serde_json::from_slice(&output)?;
```

## 7. Memory Ownership

| Scenario | Allocator | Freer |
|----------|-----------|-------|
| Plugin input (from host) | Host | Host (Extism manages) |
| Host function input (from plugin) | Plugin | Plugin (call `free` after) |
| Host function output (from host) | Host | Extism (on plugin reset) |
| Plugin output (to host) | Plugin | Host (Extism manages) |

**Rule**: Caller allocates, caller frees (for host function inputs).

## 8. Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Haskell panic = opaque error | Catch at export top level, format as JSON error |
| Memory leaks in long-running plugins | Explicit `free` after `callHost` |
| Large binary size | `wasm-opt -O3`, minimize dependencies |
| quicktype encoding mismatch | Post-process or manually write instances |
| Blocking Tokio reactor | Always use `spawn_blocking` for plugin calls |
