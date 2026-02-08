use anyhow::Result;
use exomonad_core::Role;

static EMBEDDED_TL: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/wasm-guest-tl.wasm"));
static EMBEDDED_DEV: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/wasm-guest-dev.wasm"));

/// Get embedded WASM bytes for a role. Fails if WASM was not embedded at build time.
pub fn get(role: Role) -> Result<&'static [u8]> {
    let bytes = match role {
        Role::TL => EMBEDDED_TL,
        Role::Dev => EMBEDDED_DEV,
        Role::PM => anyhow::bail!("No WASM plugin for role 'pm'"),
    };
    if bytes.is_empty() {
        anyhow::bail!(
            "No embedded WASM for role '{}'. Build with `just install-all-dev`.",
            role
        );
    }
    Ok(bytes)
}
