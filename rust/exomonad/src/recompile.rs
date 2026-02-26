//! `exomonad recompile` — build WASM plugin from Haskell source via nix.

use anyhow::{bail, Context, Result};
use std::path::{Path, PathBuf};

/// Generate a temporary cabal.project.wasm that bridges core SDK packages
/// (from the flake_ref directory) with local roles (from project_dir).
fn generate_cross_repo_cabal_project(
    sdk_dir: &Path,
    project_dir: &Path,
    role: &str,
) -> Result<PathBuf> {
    let sdk = sdk_dir.canonicalize()
        .with_context(|| format!("Failed to canonicalize SDK dir: {}", sdk_dir.display()))?;
    let proj = project_dir.canonicalize()
        .with_context(|| format!("Failed to canonicalize project dir: {}", project_dir.display()))?;

    let content = format!(
        r#"-- Auto-generated cabal.project.wasm for cross-repo build
-- SDK: {sdk_display}
-- Roles: {proj_display}

packages:
    {sdk}/haskell/wasm-guest
    {sdk}/haskell/proto
    {sdk}/haskell/vendor/freer-simple
    {sdk}/haskell/vendor/exomonad-pdk
    {sdk}/haskell/vendor/proto3-runtime
    {sdk}/haskell/vendor/proto3-wire
    {proj}/.exo/roles/{role}

shared: True
executable-dynamic: False

package *
  ghc-options: -O1

repository head.hackage
  url: https://ghc.gitlab.haskell.org/head.hackage/
  secure: True

if arch(wasm32)
  constraints: time >= 1.14
  package *
    ghc-options: -O1

allow-newer: all
"#,
        sdk_display = sdk.display(),
        proj_display = proj.display(),
        sdk = sdk.display(),
        proj = proj.display(),
        role = role,
    );

    let out_path = proj.join(".exo").join("cabal.project.wasm");
    std::fs::create_dir_all(out_path.parent().unwrap())?;
    std::fs::write(&out_path, content)
        .with_context(|| format!("Failed to write {}", out_path.display()))?;

    tracing::info!("Generated cross-repo cabal project at {}", out_path.display());
    Ok(out_path)
}

/// Build WASM for a role via nix and copy artifact to `.exo/wasm/`.
pub async fn run_recompile(role: &str, project_dir: &Path, flake_ref: Option<&str>) -> Result<()> {
    // Preflight: check nix is available
    let nix_check = std::process::Command::new("nix").arg("--version").output();
    match nix_check {
        Ok(output) if output.status.success() => {}
        _ => {
            bail!("nix is required but not found in PATH. Install nix: https://nixos.org/download")
        }
    }

    let flake_ref_str = flake_ref.unwrap_or(".");
    tracing::info!("Using flake ref: {}", flake_ref_str);
    println!("Building WASM for role '{role}'...");

    let nix_flake_arg = format!("{}#wasm", flake_ref_str);

    let project_file_arg = if let Some(ref flake) = flake_ref {
        let sdk_dir = Path::new(flake);
        let cabal_path = generate_cross_repo_cabal_project(sdk_dir, project_dir, role)?;
        format!("--project-file={}", cabal_path.display())
    } else {
        "--project-file=cabal.project.wasm".to_string()
    };

    let status = std::process::Command::new("nix")
        .args([
            "develop",
            &nix_flake_arg,
            "-c",
            "wasm32-wasi-cabal",
            "build",
            &project_file_arg,
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

    // Copy to .exo/wasm/
    let dest_dir = project_dir.join(".exo/wasm");
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

    println!("WASM built: .exo/wasm/{artifact_name} ({} bytes)", size);

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
