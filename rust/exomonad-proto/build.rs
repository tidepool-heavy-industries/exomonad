use std::io::Result;

fn main() -> Result<()> {
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

    // Skip serializing empty strings for context/detail fields
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
            "#[serde(skip_serializing_if = \"String::is_empty\")]",
        );
    }

    // ========================================================================
    // Compile all proto files
    // ========================================================================
    // Collect all proto files that exist
    let proto_files: Vec<&str> = [
        "../../proto/exomonad/ffi.proto",
        "../../proto/exomonad/common.proto",
        "../../proto/exomonad/hook.proto",
        "../../proto/exomonad/agent.proto",
        "../../proto/exomonad/popup.proto",
    ]
    .into_iter()
    .filter(|path| std::path::Path::new(path).exists())
    .collect();

    config.compile_protos(&proto_files, &["../../proto/"])?;

    Ok(())
}
