//! Build script for exomonad-core.
//!
//! Generates typed effect traits and dispatch functions from proto service
//! definitions. Reads the descriptor set produced by exomonad-proto's build
//! (via Cargo `links` metadata) instead of running protoc itself.

use std::fs;
use std::io::{Result, Write};

fn main() -> Result<()> {
    generate_effect_traits()?;
    Ok(())
}

/// Service definition extracted from proto file descriptor.
struct ServiceDef {
    /// Rust module path in exomonad_proto::effects (e.g., "git", "github")
    module: String,
    /// Trait name (e.g., "GitEffects")
    trait_name: String,
    /// Handler namespace (e.g., "git")
    namespace: String,
    /// RPC methods
    methods: Vec<MethodDef>,
}

struct MethodDef {
    /// Rust method name (snake_case, e.g., "get_branch")
    name: String,
    /// Effect type suffix (e.g., "get_branch")
    effect_suffix: String,
    /// Request type (simple name, e.g., "GetBranchRequest")
    request_type: String,
    /// Response type (simple name, e.g., "GetBranchResponse")
    response_type: String,
}

fn generate_effect_traits() -> Result<()> {
    use prost::Message;

    let out_dir = std::env::var("OUT_DIR").unwrap();

    // Read the descriptor set from exomonad-proto.
    // exomonad-proto emits this via `links = "exomonad_effects_proto"` +
    // `cargo:EFFECTS_DESCRIPTOR=<path>` in its build script.
    let descriptor_path = match std::env::var("DEP_EXOMONAD_EFFECTS_PROTO_EFFECTS_DESCRIPTOR") {
        Ok(path) if !path.is_empty() => path,
        _ => {
            println!("cargo:warning=Effects descriptor not available from exomonad-proto, skipping trait generation");
            let traits_path = format!("{}/effect_traits.rs", out_dir);
            fs::write(&traits_path, "// Effects descriptor not available\n")?;
            return Ok(());
        }
    };

    let descriptor_bytes = fs::read(&descriptor_path)?;

    let file_descriptor_set = prost_types::FileDescriptorSet::decode(&*descriptor_bytes)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string()))?;

    // Parse services from descriptor
    let services = parse_services(&file_descriptor_set);

    // Generate trait file
    let traits_path = format!("{}/effect_traits.rs", out_dir);
    let mut file = fs::File::create(&traits_path)?;

    writeln!(
        file,
        "// Generated effect traits from proto service definitions."
    )?;
    writeln!(file, "//")?;
    writeln!(
        file,
        "// Each trait defines typed methods for effects in a namespace."
    )?;
    writeln!(
        file,
        "// Handlers implement these traits for type safety. The handler's"
    )?;
    writeln!(
        file,
        "// `EffectHandler::handle` method can delegate to these typed methods."
    )?;
    writeln!(file)?;
    writeln!(file, "mod effect_traits {{")?;
    // Derive module imports from parsed services
    let modules: Vec<&str> = services.iter().map(|s| s.module.as_str()).collect();
    writeln!(
        file,
        "    use exomonad_proto::effects::{{{}}};",
        modules.join(", ")
    )?;
    writeln!(
        file,
        "    use super::{{EffectContext, EffectResult, EffectError}};"
    )?;
    writeln!(file, "    use async_trait::async_trait;")?;
    writeln!(file)?;

    for service in &services {
        generate_trait(&mut file, service)?;
        writeln!(file)?;
        generate_dispatch_helper(&mut file, service)?;
        writeln!(file)?;
    }

    // Close the module
    writeln!(file, "}} // mod effect_traits")?;
    writeln!(file)?;
    writeln!(file, "pub use effect_traits::*;")?;

    Ok(())
}

fn parse_services(fds: &prost_types::FileDescriptorSet) -> Vec<ServiceDef> {
    let mut services = Vec::new();

    for file in &fds.file {
        let package = file.package.as_deref().unwrap_or("");

        for service in &file.service {
            let service_name = service.name.as_deref().unwrap_or("");

            // Extract namespace from package (e.g., "exomonad.effects.git" -> "git")
            let namespace = package.split('.').next_back().unwrap_or("").to_string();
            if namespace.is_empty() || namespace == "effects" {
                continue;
            }

            // Module name matches namespace
            let module = namespace.clone();

            // Trait name from service name (e.g., "GitEffects")
            let trait_name = service_name.to_string();

            let methods: Vec<MethodDef> = service
                .method
                .iter()
                .filter_map(|m| {
                    let method_name = m.name.as_deref()?;
                    let input_type = m.input_type.as_deref()?;
                    let output_type = m.output_type.as_deref()?;

                    // Convert CamelCase to snake_case
                    let snake_name = to_snake_case(method_name);

                    // Extract type names (strip package prefix)
                    let request_type = input_type.rsplit('.').next()?.to_string();
                    let response_type = output_type.rsplit('.').next()?.to_string();

                    Some(MethodDef {
                        name: snake_name.clone(),
                        effect_suffix: snake_name,
                        request_type,
                        response_type,
                    })
                })
                .collect();

            if !methods.is_empty() {
                services.push(ServiceDef {
                    module,
                    trait_name,
                    namespace,
                    methods,
                });
            }
        }
    }

    services
}

fn generate_trait<W: Write>(w: &mut W, service: &ServiceDef) -> Result<()> {
    writeln!(
        w,
        "    /// Typed effect trait for the `{}.*` namespace.",
        service.namespace
    )?;
    writeln!(w, "    ///")?;
    writeln!(
        w,
        "    /// Implement this trait to handle {} effects.",
        service.namespace
    )?;
    writeln!(w, "    #[async_trait]")?;
    writeln!(w, "    pub trait {}: Send + Sync {{", service.trait_name)?;

    for method in &service.methods {
        writeln!(
            w,
            "        /// Handle `{}.{}` effect.",
            service.namespace, method.effect_suffix
        )?;
        writeln!(
            w,
            "        async fn {}(&self, req: {}::{}, ctx: &EffectContext) -> EffectResult<{}::{}>;",
            method.name, service.module, method.request_type, service.module, method.response_type
        )?;
        writeln!(w)?;
    }

    writeln!(w, "    }}")?;
    Ok(())
}

/// Generate a dispatch helper function for the trait.
///
/// Dispatch uses protobuf binary encoding: decode request bytes, call handler,
/// encode response bytes.
fn generate_dispatch_helper<W: Write>(w: &mut W, service: &ServiceDef) -> Result<()> {
    let fn_name = format!("dispatch_{}_effect", service.namespace);

    writeln!(
        w,
        "    /// Dispatch a `{}.*` effect to the appropriate typed method.",
        service.namespace
    )?;
    writeln!(w, "    ///")?;
    writeln!(
        w,
        "    /// Decodes protobuf request, calls the typed handler method,"
    )?;
    writeln!(w, "    /// and encodes the protobuf response.")?;
    writeln!(
        w,
        "    pub async fn {}<T: {}>(",
        fn_name, service.trait_name
    )?;
    writeln!(w, "        handler: &T,")?;
    writeln!(w, "        effect_type: &str,")?;
    writeln!(w, "        payload: &[u8],")?;
    writeln!(w, "        ctx: &EffectContext,")?;
    writeln!(w, "    ) -> EffectResult<Vec<u8>> {{")?;
    writeln!(w, "        use prost::Message;")?;
    writeln!(w)?;
    writeln!(w, "        let suffix = effect_type")?;
    writeln!(w, "            .strip_prefix(\"{}.\")", service.namespace)?;
    writeln!(w, "            .ok_or_else(|| EffectError::invalid_input(")?;
    writeln!(
        w,
        "                format!(\"Effect type must start with '{}.' but got: {{}}\", effect_type)",
        service.namespace
    )?;
    writeln!(w, "            ))?;")?;
    writeln!(w)?;
    writeln!(w, "        match suffix {{")?;

    for method in &service.methods {
        writeln!(w, "            \"{}\" => {{", method.effect_suffix)?;
        writeln!(
            w,
            "                let req = {}::{}::decode(payload)",
            service.module, method.request_type
        )?;
        writeln!(
            w,
            "                    .map_err(|e| EffectError::invalid_input(e.to_string()))?;"
        )?;
        writeln!(
            w,
            "                let resp = handler.{}(req, ctx).await?;",
            method.name
        )?;
        writeln!(w, "                Ok(resp.encode_to_vec())")?;
        writeln!(w, "            }}")?;
    }

    writeln!(
        w,
        "            _ => Err(EffectError::not_found(format!(\"{{}}.{{}}\", \"{}\", suffix))),",
        service.namespace
    )?;
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;

    Ok(())
}

/// Convert CamelCase to snake_case.
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        } else {
            result.push(c);
        }
    }
    result
}
