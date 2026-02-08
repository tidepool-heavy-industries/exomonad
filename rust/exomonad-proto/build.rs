use std::io::{Result, Write};
use std::path::Path;

fn main() -> Result<()> {
    // ========================================================================
    // Part 1: Core exomonad types (ffi, common, hook, agent, popup)
    // ========================================================================
    compile_core_protos()?;

    // ========================================================================
    // Part 2: Effect message types (discovered from proto/effects/*.proto)
    // ========================================================================
    compile_effect_protos()?;

    Ok(())
}

fn compile_core_protos() -> Result<()> {
    let mut config = prost_build::Config::new();

    // Add serde derives for JSON compatibility
    config.type_attribute(".", "#[derive(serde::Serialize, serde::Deserialize)]");

    // ========================================================================
    // Enum serialization: snake_case to match existing wire format
    // ========================================================================
    // FFI types
    config.type_attribute(
        ".exomonad.ffi.ErrorCode",
        "#[serde(rename_all = \"snake_case\")]",
    );

    // Hook types (Phase 2)
    config.type_attribute(
        ".exomonad.hook.HookType",
        "#[serde(rename_all = \"snake_case\")]",
    );
    config.type_attribute(
        ".exomonad.hook.StopDecision",
        "#[serde(rename_all = \"snake_case\")]",
    );

    // Agent types (Phase 2)
    config.type_attribute(
        ".exomonad.agent.AgentType",
        "#[serde(rename_all = \"snake_case\")]",
    );
    config.type_attribute(
        ".exomonad.agent.AgentStatus",
        "#[serde(rename_all = \"snake_case\")]",
    );

    // Common types (Phase 2)
    config.type_attribute(
        ".exomonad.common.Role",
        "#[serde(rename_all = \"snake_case\")]",
    );
    config.type_attribute(
        ".exomonad.common.ToolPermission",
        "#[serde(rename_all = \"snake_case\")]",
    );

    // ========================================================================
    // Note: proto3-suite doesn't support the `optional` keyword that was
    // re-added in proto3. All fields use default values (empty string,
    // 0, false, etc.) when not set. The Haskell generated code treats
    // empty/default as implicit optionality.
    //
    // For wire compatibility, we use skip_serializing_if with default
    // value checks for string and numeric fields to omit defaults.
    // ========================================================================

    // Skip serializing empty/default fields and default to empty on deserialize.
    // Proto3 scalars use empty string / 0 as default, so omitting them is safe.
    for field in [
        "ErrorContext.command",
        "ErrorContext.stderr",
        "ErrorContext.stdout",
        "ErrorContext.file_path",
        "ErrorContext.working_dir",
        "FfiError.suggestion",
    ] {
        config.field_attribute(
            &format!(".exomonad.ffi.{}", field),
            "#[serde(default, skip_serializing_if = \"String::is_empty\")]",
        );
    }

    // Skip serializing None context and default to None on deserialize
    config.field_attribute(
        ".exomonad.ffi.FfiError.context",
        "#[serde(default, skip_serializing_if = \"Option::is_none\")]",
    );

    // Collect all proto files that exist
    let proto_files: Vec<&str> = [
        "proto/exomonad/ffi.proto",
        "proto/exomonad/common.proto",
        "proto/exomonad/hook.proto",
        "proto/exomonad/agent.proto",
        "proto/exomonad/popup.proto",
    ]
    .into_iter()
    .filter(|path| Path::new(path).exists())
    .collect();

    config.compile_protos(&proto_files, &["proto/"])?;

    Ok(())
}

fn compile_effect_protos() -> Result<()> {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let mut config = prost_build::Config::new();

    // Effect types use protobuf binary encoding (not JSON).
    // No serde derives needed.

    // Map exomonad.common types to the already-generated core module
    // to avoid re-generating them without serde derives.
    config.extern_path(".exomonad.common", "crate::common");

    // Save descriptor set for downstream crates.
    // exomonad-core reads this via DEP_EXOMONAD_EFFECTS_PROTO_EFFECTS_DESCRIPTOR
    // to generate typed effect traits without running protoc itself.
    let descriptor_path = format!("{}/effects_descriptor.bin", out_dir);
    config.file_descriptor_set_path(&descriptor_path);

    // Discover all proto files in the effects directory
    let proto_dir = Path::new("proto/effects");
    if !proto_dir.exists() {
        println!("cargo:warning=Proto effects directory not found, skipping");
        generate_empty_effect_modules(&out_dir)?;
        println!("cargo:EFFECTS_DESCRIPTOR=");
        return Ok(());
    }

    let effect_proto_files: Vec<String> = std::fs::read_dir(proto_dir)?
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("proto") {
                Some(format!(
                    "proto/effects/{}",
                    entry.file_name().to_string_lossy()
                ))
            } else {
                None
            }
        })
        .collect();

    if effect_proto_files.is_empty() {
        println!("cargo:warning=No .proto files found in effects directory");
        generate_empty_effect_modules(&out_dir)?;
        println!("cargo:EFFECTS_DESCRIPTOR=");
        return Ok(());
    }

    config.compile_protos(
        &effect_proto_files,
        &["proto/", "proto/effects/"],
    )?;

    // Communicate descriptor path to dependents (exomonad-core)
    println!("cargo:EFFECTS_DESCRIPTOR={}", descriptor_path);

    // Generate effect_modules.rs from discovered output files
    generate_effect_module_declarations(&out_dir)?;

    // Rerun if proto files change
    println!("cargo:rerun-if-changed=proto/effects/");

    Ok(())
}

/// Generate effect_modules.rs by scanning OUT_DIR for prost-generated files.
///
/// prost generates one file per protobuf package:
/// - `exomonad.effects.rs` — base package (effect_error + envelope)
/// - `exomonad.effects.git.rs` — git namespace
/// - `exomonad.effects.github.rs` — github namespace
/// - etc.
///
/// We generate a `pub mod <name> { include!(...) }` for each.
fn generate_effect_module_declarations(out_dir: &str) -> Result<()> {
    let mut modules = Vec::new();

    for entry in std::fs::read_dir(out_dir)? {
        let entry = entry?;
        let name = entry.file_name().to_string_lossy().to_string();

        // Match "exomonad.effects.<namespace>.rs" (sub-namespace modules)
        // Skip "exomonad.effects.rs" (base package — handled as `error` module)
        if let Some(rest) = name.strip_prefix("exomonad.effects.") {
            if let Some(module_name) = rest.strip_suffix(".rs") {
                if !module_name.is_empty() && !module_name.contains('.') {
                    modules.push(module_name.to_string());
                }
            }
        }
    }

    modules.sort();

    let path = format!("{}/effect_modules.rs", out_dir);
    let mut file = std::fs::File::create(&path)?;

    writeln!(
        file,
        "// Generated from proto/effects/*.proto — do not edit."
    )?;
    writeln!(file)?;

    // Base package types (effect_error.proto + envelope.proto → exomonad.effects.rs)
    writeln!(file, "/// Effect error and envelope types.")?;
    writeln!(file, "pub mod error {{")?;
    writeln!(
        file,
        "    include!(concat!(env!(\"OUT_DIR\"), \"/exomonad.effects.rs\"));"
    )?;
    writeln!(file, "}}")?;
    writeln!(file)?;

    // One module per effect namespace
    for module in &modules {
        writeln!(file, "/// `{}.*` effect types.", module)?;
        writeln!(file, "pub mod {} {{", module)?;
        writeln!(
            file,
            "    include!(concat!(env!(\"OUT_DIR\"), \"/exomonad.effects.{}.rs\"));",
            module
        )?;
        writeln!(file, "}}")?;
        writeln!(file)?;
    }

    Ok(())
}

fn generate_empty_effect_modules(out_dir: &str) -> Result<()> {
    let path = format!("{}/effect_modules.rs", out_dir);
    std::fs::write(&path, "// No effect proto files found.\n")?;
    Ok(())
}
