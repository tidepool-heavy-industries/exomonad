//! The policy contract's core: the [`Tool`] trait, the JSON-edge helpers every tool's
//! hand-written adapter calls (`parse` / `ok_json` / `schema_json`), and [`ToolOutput`].
//!
//! A tool is a **type**: an `Args` struct (deriving `Deserialize + JsonSchema`), a
//! generic-over-caps `run` whose cap bounds *are* its least-privilege spec, and a ~6-line
//! hand-written `Tool<R>` adapter (NO macro — the locked rule) that monomorphizes `run`
//! at the concrete runtime `R` and erases args↔JSON.

use exo_caps::{CapError, CapResult};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::Value;
use std::future::Future;
use std::pin::Pin;

/// A boxed, `Send` future — the return shape of the async hook/event fn-pointers stored in
/// [`RoleDef`](crate::roles::RoleDef). Lets the role table stay a greppable struct-of-fn-
/// pointers (the doc's chosen form) while the hooks do async cap IO.
pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + Send + 'a>>;

/// What a tool's `run` returns. Plain text plus optional structured data; the sidecar (N1)
/// maps this to the rmcp `CallToolResult`. Kept minimal — a richer content model is a
/// Wave-2 concern, not policy's.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ToolOutput {
    pub text: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

impl ToolOutput {
    /// A text-only result.
    pub fn text(s: impl Into<String>) -> Self {
        ToolOutput {
            text: s.into(),
            data: None,
        }
    }
    /// Text plus a structured payload.
    pub fn with_data(s: impl Into<String>, data: Value) -> Self {
        ToolOutput {
            text: s.into(),
            data: Some(data),
        }
    }
}

/// An MCP tool, **object-safe over the concrete runtime `R`** (`Vec<Box<dyn Tool<R>>>`).
/// `R` is the concrete type that impls the cap traits — NOT `dyn Caps`. The caps a tool
/// needs are expressed as bounds on its `run` and surfaced in the adapter's `impl` header,
/// so least-privilege is compiler-checked per tool.
#[async_trait::async_trait]
pub trait Tool<R: Send + Sync>: Send + Sync {
    /// The MCP tool name (the wire identifier).
    fn name(&self) -> &str;
    /// A one-line description of what the tool does and where it sits in the local-merge loop.
    /// Surfaced in MCP `tools/list` so the toolset is self-documenting — an agent learns the
    /// convergence model (commit → `submit_branch` → parent `merge`, no PR/remote) from the
    /// tools it actually has, not from out-of-band instructions.
    fn description(&self) -> &str;
    /// The JSON Schema for this tool's arguments — derived from `Args` (single source).
    fn schema(&self) -> Value;
    /// Dispatch: erase JSON → call the typed `run` → erase the result back to JSON.
    async fn call(&self, ctx: &R, args: Value) -> CapResult<Value>;
}

/// Adapter helper: parse a tool's JSON arguments into its typed `Args`.
pub fn parse<T: DeserializeOwned>(j: Value) -> CapResult<T> {
    serde_json::from_value(j).map_err(|e| CapError::Json {
        context: "tool arguments".into(),
        source: e,
    })
}

/// Adapter helper: serialize a [`ToolOutput`] back to the JSON edge.
pub fn ok_json(out: ToolOutput) -> CapResult<Value> {
    serde_json::to_value(out).map_err(|e| CapError::Json {
        context: "tool output".into(),
        source: e,
    })
}

/// Adapter helper: a tool's `schema()` body — the JSON Schema for `T`, generated **inline**
/// (`inline_subschemas`), so a nested type produces a self-contained object schema with **no
/// `$ref` / `definitions`**. Anthropic resolves draft-07 `$ref`s fine, but stricter tool-schema
/// validators reject them (Moonshot/Kimi only accept `#/$defs/` refs) — inlining sidesteps the
/// dialect difference entirely, so the same tool works on Claude and on a proxy-backed brain.
/// Usage: `fn schema(&self) -> Value { schema_json::<MyArgs>() }`.
pub fn schema_json<T: schemars::JsonSchema>() -> Value {
    let settings = schemars::gen::SchemaSettings::draft07().with(|s| s.inline_subschemas = true);
    let root = settings.into_generator().into_root_schema_for::<T>();
    serde_json::to_value(root).expect("a derived JSON Schema always serializes")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tool_output_text_omits_null_data() {
        let j = serde_json::to_value(ToolOutput::text("hi")).unwrap();
        assert_eq!(j, serde_json::json!({ "text": "hi" }));
    }

    #[test]
    fn parse_round_trips_and_reports_bad_args() {
        #[derive(serde::Deserialize)]
        struct A {
            n: u32,
        }
        let a: A = parse(serde_json::json!({ "n": 5 })).unwrap();
        assert_eq!(a.n, 5);
        assert!(parse::<A>(serde_json::json!({ "n": "not-a-number" })).is_err());
    }

    #[test]
    fn schema_json_inlines_nested_structs_no_refs() {
        // A tool schema with a nested struct must be self-contained: NO `$ref` / `definitions`.
        // schemars' default (draft-07) emits `#/definitions/...` refs, which Anthropic resolves but
        // Moonshot/Kimi reject — inlining keeps the same tool usable on every backend.
        #[derive(schemars::JsonSchema)]
        struct Inner {
            x: u32,
        }
        #[derive(schemars::JsonSchema)]
        struct Outer {
            items: Vec<Inner>,
        }
        let text = serde_json::to_string(&schema_json::<Outer>()).unwrap();
        assert!(!text.contains("$ref"), "schema must be ref-free: {text}");
        assert!(
            !text.contains("definitions"),
            "schema must carry no definitions map: {text}"
        );
        assert!(
            text.contains("items"),
            "the nested array is still present: {text}"
        );
    }
}
