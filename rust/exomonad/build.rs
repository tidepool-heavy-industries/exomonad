//! Build script for exomonad: embeds git revision info at compile time.
//!
//! Priority:
//!   1. VERGEN_GIT_SHA env var (set by Docker build)
//!   2. git rev-parse (local development)
//!   3. Build failure (no silent "unknown")

use std::env;
use std::process::Command;

fn main() {
    // Rerun if git HEAD changes (for local dev)
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/refs/heads");
    // Rerun if env var changes (for Docker builds)
    println!("cargo:rerun-if-env-changed=VERGEN_GIT_SHA");

    // Check for pre-set env var first (Docker builds set this)
    let git_sha = env::var("VERGEN_GIT_SHA")
        .ok()
        .filter(|s| !s.is_empty())
        .or_else(|| {
            // Fall back to git command (local development)
            Command::new("git")
                .args(["rev-parse", "--short", "HEAD"])
                .output()
                .ok()
                .and_then(|o| {
                    if o.status.success() {
                        String::from_utf8(o.stdout)
                            .ok()
                            .map(|s| s.trim().to_string())
                    } else {
                        None
                    }
                })
        })
        .expect("GIT_SHA required: set VERGEN_GIT_SHA env var or build from a git repo");

    println!("cargo:rustc-env=VERGEN_GIT_SHA={git_sha}");
}
