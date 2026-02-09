//! `exomonad recompile` — build WASM plugin from Haskell source via nix.

use anyhow::{bail, Context, Result};
use std::path::Path;

/// Build WASM for a role via nix and copy artifact to `.exomonad/wasm/`.
pub async fn run_recompile(role: &str, project_dir: &Path) -> Result<()> {
    // Preflight: check nix is available
    let nix_check = std::process::Command::new("nix").arg("--version").output();
    match nix_check {
        Ok(output) if output.status.success() => {}
        _ => {
            bail!("nix is required but not found in PATH. Install nix: https://nixos.org/download")
        }
    }

    println!("Building WASM for role '{role}'...");

    let status = std::process::Command::new("nix")
        .args([
            "develop",
            ".#wasm",
            "-c",
            "wasm32-wasi-cabal",
            "build",
            &format!("--project-file=cabal.project.wasm"),
            &format!("wasm-guest-{role}"),
        ])
        .current_dir(project_dir)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .status()
        .context("Failed to execute nix develop")?;

    if !status.success() {
        bail!(
            "WASM build failed with exit code: {}",
            status.code().unwrap_or(-1)
        );
    }

    // Find the WASM artifact in dist-newstyle
    let artifact_name = format!("wasm-guest-{role}.wasm");
    let dist_dir = project_dir.join("dist-newstyle");

    let wasm_path = find_wasm_artifact(&dist_dir, &artifact_name)?;

    // Copy to .exomonad/wasm/
    let dest_dir = project_dir.join(".exomonad/wasm");
    std::fs::create_dir_all(&dest_dir)
        .with_context(|| format!("Failed to create {}", dest_dir.display()))?;

    let dest_path = dest_dir.join(&artifact_name);
    std::fs::copy(&wasm_path, &dest_path).with_context(|| {
        format!(
            "Failed to copy {} -> {}",
            wasm_path.display(),
            dest_path.display()
        )
    })?;

    let size = std::fs::metadata(&dest_path).map(|m| m.len()).unwrap_or(0);

    println!(
        "WASM built: .exomonad/wasm/{artifact_name} ({} bytes)",
        size
    );

    Ok(())
}

/// Recursively search `dist-newstyle/` for the WASM artifact.
fn find_wasm_artifact(dist_dir: &Path, artifact_name: &str) -> Result<std::path::PathBuf> {
    if !dist_dir.exists() {
        bail!(
            "dist-newstyle/ not found at {}. Has the WASM build run?",
            dist_dir.display()
        );
    }

    // Walk the directory tree looking for the artifact
    let mut found = None;
    walk_dir(dist_dir, artifact_name, &mut found)?;

    found.ok_or_else(|| {
        anyhow::anyhow!("Could not find {} in {}", artifact_name, dist_dir.display())
    })
}

fn walk_dir(dir: &Path, target: &str, found: &mut Option<std::path::PathBuf>) -> Result<()> {
    if found.is_some() {
        return Ok(());
    }

    let entries = std::fs::read_dir(dir)
        .with_context(|| format!("Failed to read directory: {}", dir.display()))?;

    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            walk_dir(&path, target, found)?;
            if found.is_some() {
                return Ok(());
            }
        } else if path.file_name().map(|n| n == target).unwrap_or(false) {
            *found = Some(path);
            return Ok(());
        }
    }

    Ok(())
}
