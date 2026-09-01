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

    crate::init::ensure_claude_rules(&cwd)?;

    info!("Project initialized. Run `exomonad init` to start a session.");
    Ok(())
}

#[cfg(test)]
mod tests {
    const DISTRIBUTED_RULES_TEMPLATE: &str = include_str!("../../../.exo/rules/exomonad.md");

    #[test]
    fn distributed_rules_are_portable_v2_guidance() {
        assert!(DISTRIBUTED_RULES_TEMPLATE.contains("# ExoMonad Agent Rules"));
        assert!(DISTRIBUTED_RULES_TEMPLATE.contains("Explicit instructions"));
        assert!(DISTRIBUTED_RULES_TEMPLATE.contains("submit_branch"));
        assert!(!DISTRIBUTED_RULES_TEMPLATE.contains("rust/"));
        assert!(!DISTRIBUTED_RULES_TEMPLATE
            .contains("active mechanics and role matrix live in `.claude/rules/exomonad.md`"));
    }
}
