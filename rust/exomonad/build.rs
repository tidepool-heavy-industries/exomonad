//! Build script for exomonad: embeds git revision info at compile time.
//!
//! Uses a simple approach: capture git rev-parse output directly.
//! Falls back to "unknown" if git is not available or not in a repo.

use std::process::Command;

fn main() {
    // Rerun if git HEAD changes
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=.git/refs/heads");

    // Get git SHA
    let git_sha = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                String::from_utf8(o.stdout).ok()
            } else {
                None
            }
        })
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    println!("cargo:rustc-env=VERGEN_GIT_SHA={git_sha}");
}
