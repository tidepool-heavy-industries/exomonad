# ExoMonad Effects Integration Guide

How to use and extend the ExoMonad effects system from a consuming project.

## What This Is

ExoMonad effects are typed, namespace-routed operations that cross the Haskell WASM / Rust host boundary using protobuf binary encoding. Your Haskell WASM code yields effects; the Rust runtime executes them.

```
Haskell WASM Guest                         Rust Host
─────────────────                         ─────────
runEffect @GitGetBranch req               EffectRegistry
  │                                            │
  │  effectId = "git.get_branch"               │
  │  encode req as protobuf binary             │
  ▼                                            │
EffectEnvelope                                 │
  { effect_type: "git.get_branch"              │
  , payload: <protobuf bytes>  }               │
  │                                            │
  │──── yield_effect FFI ─────────────────────▶│
  │                                            │
  │                              namespace = "git"
  │                              suffix = "get_branch"
  │                                            │
  │                              GitHandler.get_branch(req)
  │                                            │
  │◀── EffectResponse ────────────────────────│
  │    { payload: <protobuf bytes> }           │
  ▼                                            │
decode response                                │
Right GetBranchResponse { branch = "main" }
```

One host function (`yield_effect`) routes all effects. The `effect_type` string (e.g., `"git.get_branch"`) determines which handler runs. Proto files are the single source of truth for request/response types on both sides.

## Using Built-in Effects

### Available Namespaces

| Namespace | Module | Operations |
|-----------|--------|------------|
| `git` | `ExoMonad.Effects.Git` | get_branch, get_status, get_commits, has_unpushed_commits, get_remote_url, get_repo_info, get_worktree |
| `github` | `ExoMonad.Effects.GitHub` | list_issues, get_issue, list_pull_requests, get_pull_request, get_pull_request_for_branch, create_pull_request |
| `log` | `ExoMonad.Effects.Log` | info, error, debug, warn, emit_event |
| `fs` | (proto defined, Haskell wrapper pending) | read_file, write_file, file_exists, list_directory, delete_file |
| `agent` | (proto defined, Haskell wrapper pending) | spawn, spawn_batch, cleanup, cleanup_batch, cleanup_merged, list |

### Invoking Effects

Each effect has a **phantom type**, an **Effect instance**, and a **smart constructor**:

```haskell
import ExoMonad.Effects.Git

-- Smart constructor (preferred)
result <- getBranch (GetBranchRequest { getBranchRequestWorkingDir = "." })
case result of
  Right resp -> putStrLn $ "Branch: " <> show (getBranchResponseBranch resp)
  Left err   -> handleError err

-- Or use runEffect directly with type application
import ExoMonad.Effect.Class (runEffect)

result <- runEffect @GitGetBranch (GetBranchRequest { getBranchRequestWorkingDir = "." })
```

For fire-and-forget effects (where you only care about success/failure):

```haskell
import ExoMonad.Effects.Log

result <- logInfo (InfoRequest { infoRequestMessage = "Starting process"
                               , infoRequestFields = "" })
-- result :: Either EffectError LogResponse
```

### Request/Response Types

All request and response types are proto-generated. Field names follow the proto3-suite convention: `messageName` + `FieldName`. For example, `GetBranchRequest` with field `working_dir` becomes `getBranchRequestWorkingDir`.

Proto types use:
- `Data.Text.Lazy.Text` for strings
- `Data.ByteString.ByteString` for bytes
- `Data.Int.Int32` / `Int64` for integers
- `Maybe a` for optional fields
- `Data.Vector.Vector a` for repeated fields
- Sum types for `oneof` fields

### Error Handling

All effects return `Either EffectError response`. The error type is a discriminated union:

```haskell
import Effects.EffectError

case result of
  Left (EffectError (Just (EffectErrorKindNotFound nf))) ->
    putStrLn $ "Not found: " <> show (notFoundResource nf)
  Left (EffectError (Just (EffectErrorKindInvalidInput ii))) ->
    putStrLn $ "Bad input: " <> show (invalidInputMessage ii)
  Left (EffectError (Just (EffectErrorKindNetworkError ne))) ->
    putStrLn $ "Network: " <> show (networkErrorMessage ne)
  Left (EffectError (Just (EffectErrorKindPermissionDenied pd))) ->
    putStrLn $ "Denied: " <> show (permissionDeniedMessage pd)
  Left (EffectError (Just (EffectErrorKindTimeout to))) ->
    putStrLn $ "Timeout: " <> show (timeoutMessage to)
  Left (EffectError (Just (EffectErrorKindCustom c))) ->
    putStrLn $ "Custom [" <> show (customCode c) <> "]: " <> show (customMessage c)
  Left (EffectError Nothing) ->
    putStrLn "Unknown error"
  Right resp ->
    use resp
```

## Adding a New Effect Namespace

The full pipeline: proto file -> codegen -> Haskell wrapper -> Rust handler.

### Step 1: Define the Proto File

Create `proto/effects/myns.proto`:

```protobuf
syntax = "proto3";
package exomonad.effects.myns;

import "effects/effect_error.proto";

// Service definition drives Rust codegen (trait + dispatch function).
// Each rpc becomes a method on the generated trait.
service MyEffects {
  rpc DoSomething(DoSomethingRequest) returns (DoSomethingResponse);
  rpc GetState(GetStateRequest) returns (GetStateResponse);
}

message DoSomethingRequest {
  string input = 1;
  int32 count = 2;
}

message DoSomethingResponse {
  string output = 1;
  bool success = 2;
}

message GetStateRequest {
  string key = 1;
}

message GetStateResponse {
  string value = 1;
}
```

The `service` block is what drives Rust codegen. Each `rpc` maps to:
- A method on the generated `MyEffects` trait
- A case in the generated `dispatch_myns_effect` function
- An effect ID: `"myns.do_something"`, `"myns.get_state"`

RPC names are converted from PascalCase to snake_case for effect IDs.

### Step 2: Generate Types

```bash
just proto-gen    # Haskell types via proto3-suite
cargo build -p exomonad-proto --features effects   # Rust types via prost
cargo build -p exomonad-runtime   # Rust trait + dispatch via build.rs
```

This produces:
- **Haskell:** `haskell/proto/src/Effects/Myns.hs` with `Message` instances
- **Rust:** `exomonad_proto::effects::myns::*` with prost types
- **Rust:** `MyEffects` trait and `dispatch_myns_effect` function (generated by `exomonad-runtime/build.rs`)

### Step 3: Create Haskell Effect Wrapper

Create `haskell/wasm-guest/src/ExoMonad/Effects/Myns.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Myns
  ( -- * Effect Types
    MyDoSomething,
    MyGetState,

    -- * Smart Constructors
    doSomething,
    getState,

    -- * Re-exported proto types
    module Effects.Myns,
  )
where

import Effects.EffectError (EffectError)
import Effects.Myns
import ExoMonad.Effect.Class (Effect (..), runEffect)

data MyDoSomething

instance Effect MyDoSomething where
  type Input MyDoSomething = DoSomethingRequest
  type Output MyDoSomething = DoSomethingResponse
  effectId = "myns.do_something"

data MyGetState

instance Effect MyGetState where
  type Input MyGetState = GetStateRequest
  type Output MyGetState = GetStateResponse
  effectId = "myns.get_state"

-- Smart constructors

doSomething :: DoSomethingRequest -> IO (Either EffectError DoSomethingResponse)
doSomething = runEffect @MyDoSomething

getState :: GetStateRequest -> IO (Either EffectError GetStateResponse)
getState = runEffect @MyGetState
```

The pattern is mechanical:
1. One phantom type per RPC
2. Effect instance mapping Input/Output to proto types and effectId to `"namespace.rpc_name"`
3. Smart constructor that calls `runEffect @PhantomType`
4. Re-export the proto module so consumers get the request/response types

### Step 4: Register in wasm-guest.cabal

Add to `exposed-modules` in the `wasm-guest-internal` library:

```cabal
ExoMonad.Effects.Myns
```

Add `Effects.Myns` to `exomonad-proto.cabal` if the codegen script didn't already.

### Step 5: Implement the Rust Handler

Create `rust/exomonad-runtime/src/handlers/myns.rs`:

```rust
use crate::effects::{dispatch_myns_effect, EffectError, EffectHandler, EffectResult, MyEffects};
use async_trait::async_trait;
use exomonad_proto::effects::myns::*;

pub struct MyHandler {
    // inject whatever services you need
}

impl MyHandler {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl EffectHandler for MyHandler {
    fn namespace(&self) -> &str {
        "myns"
    }

    async fn handle(&self, effect_type: &str, payload: &[u8]) -> EffectResult<Vec<u8>> {
        dispatch_myns_effect(self, effect_type, payload).await
    }
}

#[async_trait]
impl MyEffects for MyHandler {
    async fn do_something(&self, req: DoSomethingRequest) -> EffectResult<DoSomethingResponse> {
        // Your implementation here
        Ok(DoSomethingResponse {
            output: format!("processed: {}", req.input),
            success: true,
        })
    }

    async fn get_state(&self, req: GetStateRequest) -> EffectResult<GetStateResponse> {
        // Your implementation here
        Ok(GetStateResponse {
            value: String::new(),
        })
    }
}
```

The `dispatch_myns_effect` function (auto-generated by `build.rs`) handles:
- Splitting the effect_type to get the suffix
- Decoding the protobuf payload into the correct request type
- Calling the right trait method
- Encoding the response back to protobuf bytes

### Step 6: Register the Handler

In your runtime setup code, register the handler with the `EffectRegistry`:

```rust
let mut registry = EffectRegistry::new();
registry.register(Arc::new(MyHandler::new()));
// The handler's namespace() return value ("myns") is used for routing
```

## Architecture Reference

### Effect ID Convention

Effect IDs follow `namespace.operation_name`:
- `git.get_branch`
- `github.list_issues`
- `log.info`
- `myns.do_something`

The namespace prefix routes to the correct `EffectHandler`. The suffix routes to the correct method within that handler.

### Wire Format

Protobuf binary throughout. No JSON at the effect boundary.

```
Request path:
  typed request → protobuf encode → EffectEnvelope → protobuf encode → WASM memory → host function

Response path:
  host function → WASM memory → protobuf decode → EffectResponse → extract payload → protobuf decode → typed response
```

### Trait Generation (Rust build.rs)

`exomonad-runtime/build.rs` reads proto service definitions and generates:

```rust
// Generated trait (one method per RPC)
#[async_trait]
pub trait MyEffects: Send + Sync {
    async fn do_something(&self, req: DoSomethingRequest) -> EffectResult<DoSomethingResponse>;
    async fn get_state(&self, req: GetStateRequest) -> EffectResult<GetStateResponse>;
}

// Generated dispatch function (binary decode → method → binary encode)
pub async fn dispatch_myns_effect<T: MyEffects>(
    handler: &T,
    effect_type: &str,
    payload: &[u8],
) -> EffectResult<Vec<u8>> {
    let suffix = effect_type.strip_prefix("myns.").unwrap_or(effect_type);
    match suffix {
        "do_something" => {
            let req = DoSomethingRequest::decode(payload)
                .map_err(|e| EffectError::invalid_input(e.to_string()))?;
            let resp = handler.do_something(req).await?;
            Ok(resp.encode_to_vec())
        }
        "get_state" => {
            let req = GetStateRequest::decode(payload)
                .map_err(|e| EffectError::invalid_input(e.to_string()))?;
            let resp = handler.get_state(req).await?;
            Ok(resp.encode_to_vec())
        }
        _ => Err(EffectError::not_found(format!("Unknown effect: {}", effect_type))),
    }
}
```

You never write dispatch boilerplate by hand. Define the proto service, run codegen, implement the trait.

### Dependencies

**Haskell (wasm-guest consumer):**
```cabal
build-depends:
    exomonad-proto    -- proto-generated types (Effects.*, ExoMonad.*)
    proto3-runtime    -- Proto3.Suite.Class (Message, encode, decode)
```

**Rust (runtime consumer):**
```toml
[dependencies]
exomonad-proto = { path = "../exomonad-proto", features = ["effects"] }
async-trait = "0.1"
prost = "0.13"
```

### Haskell Module Map

```
ExoMonad.Effect.Class         -- Effect typeclass, runEffect, runEffect_
ExoMonad.Guest.Effect         -- yieldEffect (low-level), EffectError re-export
ExoMonad.Effects.Git          -- Git effect types + smart constructors
ExoMonad.Effects.GitHub       -- GitHub effect types + smart constructors
ExoMonad.Effects.Log          -- Log effect types + smart constructors
Effects.Git                   -- Proto-generated Git request/response types
Effects.Github                -- Proto-generated GitHub request/response types
Effects.Log                   -- Proto-generated Log request/response types
Effects.EffectError           -- Proto-generated error types
Effects.Envelope              -- Proto-generated EffectEnvelope/EffectResponse
```

`ExoMonad.Effects.*` modules re-export their corresponding `Effects.*` proto module, so you typically only need one import:

```haskell
import ExoMonad.Effects.Git  -- gets both effect types AND proto types
```

### Rust Module Map

```
exomonad_proto::effects::git       -- prost-generated Git types
exomonad_proto::effects::github    -- prost-generated GitHub types
exomonad_proto::effects::log       -- prost-generated Log types
exomonad_proto::effects::envelope  -- EffectEnvelope, EffectResponse
exomonad_proto::effects::effect_error -- EffectError

exomonad_runtime::effects          -- EffectHandler trait, EffectRegistry
exomonad_runtime::effects          -- Generated: GitEffects trait, dispatch_git_effect, etc.
exomonad_runtime::handlers         -- Handler implementations
```
