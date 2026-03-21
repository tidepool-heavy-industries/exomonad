use anyhow::{Context, Result};
use exomonad::config::Config;
use std::path::PathBuf;
use tracing::{info, warn};

/// Initialize a new exomonad project in the current directory.
/// Creates .exo/config.toml, .gitignore entries, copies WASM, and rules template.
pub async fn run(_name: Option<String>) -> Result<()> {
    let cwd = std::env::current_dir()?;
    let config_path = cwd.join(".exo/config.toml");

    if config_path.exists() {
        anyhow::bail!("ExoMonad project already exists (found .exo/config.toml)");
    }

    info!("Initializing new ExoMonad project");
    std::fs::create_dir_all(cwd.join(".exo"))?;
    std::fs::write(
        &config_path,
        "# ExoMonad project config
# All fields are optional — see docs for overrides
",
    )?;

    // Add gitignore entries
    crate::init::ensure_gitignore(&cwd)?;

    // Resolve config
    let config = Config::discover()?;

    // Copy WASM if it doesn't exist yet (same logic as init.rs)
    let wasm_filename = format!("wasm-guest-{}.wasm", config.wasm_name);
    let wasm_path = config.wasm_dir.join(&wasm_filename);
    if !wasm_path.exists() {
        let roles_dir = cwd.join(".exo/roles");
        if roles_dir.is_dir() {
            info!(path = %wasm_path.display(), "WASM not found, building...");
            exomonad::recompile::run_recompile(
                &config.wasm_name,
                &cwd,
                config.flake_ref.as_deref(),
            )
            .await?;
        } else if let Ok(home) = std::env::var("HOME") {
            let home = PathBuf::from(home);
            // Fall back to globally installed WASM from ~/.exo/wasm/
            let global_wasm = home.join(".exo/wasm").join(&wasm_filename);
            if global_wasm.exists() {
                info!(
                    src = %global_wasm.display(),
                    dst = %wasm_path.display(),
                    "Copying WASM from global install"
                );
                std::fs::create_dir_all(&config.wasm_dir)?;
                std::fs::copy(&global_wasm, &wasm_path)?;
            } else {
                warn!(
                    path = %wasm_path.display(),
                    "No WASM found locally or at ~/.exo/wasm/. Run 'just install-all' in the exomonad repo, or copy roles: cp -r /path/to/exomonad/.exo/roles .exo/roles"
                );
            }
        } else {
            warn!(
                path = %wasm_path.display(),
                "No WASM found locally or at ~/.exo/wasm/. Run 'just install-all' in the exomonad repo, or copy roles: cp -r /path/to/exomonad/.exo/roles .exo/roles"
            );
        }
    }

    // Write hook configuration
    let binary_path = exomonad_core::find_exomonad_binary();
    exomonad_core::hooks::HookConfig::write_persistent(&cwd, &binary_path, None, None)
        .context("Failed to write hook configuration")?;
    info!("Hook configuration written to .claude/settings.local.json");

    // Copy Claude rules template if available and not already present (same logic as init.rs)
    {
        let rules_dest = cwd.join(".claude/rules/exomonad.md");
        if !rules_dest.exists() {
            // Resolution: project-local .exo/rules/ → global ~/.exo/rules/
            let local_template = cwd.join(".exo/rules/exomonad.md");
            let global_template = std::env::var("HOME")
                .ok()
                .map(|h| PathBuf::from(h).join(".exo/rules/exomonad.md"));

            let source = if local_template.exists() {
                Some(local_template)
            } else {
                global_template.filter(|p| p.exists())
            };

            if let Some(src) = source {
                std::fs::create_dir_all(cwd.join(".claude/rules"))?;
                std::fs::copy(&src, &rules_dest)?;
                info!(
                    src = %src.display(),
                    "Copied Claude rules to .claude/rules/exomonad.md"
                );
            }
        }
    }

    info!("Project initialized. Run `exomonad init` to start a session.");
    Ok(())
}
