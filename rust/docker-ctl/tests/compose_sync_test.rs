//! Integration test: verify domain.rs volume constants match docker-compose.yml
//!
//! This test parses docker-compose.yml and verifies that all volume names
//! defined in domain.rs exist in the compose file's volumes section.
//!
//! Prevents drift between Rust code and Docker Compose configuration.

use serde_yaml::Value;
use std::collections::HashSet;

/// Read and parse docker-compose.yml from repo root
fn parse_compose() -> Value {
    let compose_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../docker-compose.yml");
    let content = std::fs::read_to_string(compose_path)
        .expect("docker-compose.yml should exist at repo root");
    serde_yaml::from_str(&content).expect("docker-compose.yml should be valid YAML")
}

/// Extract all volume names from the top-level volumes section
fn extract_volume_names(compose: &Value) -> HashSet<String> {
    let volumes = compose
        .get("volumes")
        .expect("docker-compose.yml should have volumes section");

    match volumes {
        Value::Mapping(map) => map
            .keys()
            .filter_map(|k| k.as_str().map(|s| s.to_string()))
            .collect(),
        _ => HashSet::new(),
    }
}

#[test]
fn domain_rs_volumes_exist_in_compose() {
    let compose = parse_compose();
    let compose_volumes = extract_volume_names(&compose);

    // Volume names from domain.rs - must match VolumeMount constants
    let domain_volumes = vec![
        "exomonad-repo",
        "exomonad-worktrees",
        "exomonad-sockets",
        "exomonad-gh-auth",
        "exomonad-claude-auth",
    ];

    let mut missing = Vec::new();
    for vol in &domain_volumes {
        if !compose_volumes.contains(*vol) {
            missing.push(*vol);
        }
    }

    assert!(
        missing.is_empty(),
        "domain.rs defines volumes that don't exist in docker-compose.yml: {:?}\n\
         Compose has: {:?}",
        missing,
        compose_volumes
    );
}

#[test]
fn compose_has_no_orphan_exomonad_volumes() {
    let compose = parse_compose();
    let compose_volumes = extract_volume_names(&compose);

    // All exomonad-* volumes should be accounted for in domain.rs
    let domain_volumes: HashSet<_> = vec![
        "exomonad-repo",
        "exomonad-worktrees",
        "exomonad-sockets",
        "exomonad-gh-auth",
        "exomonad-claude-auth",
        // Additional volumes not managed by domain.rs (e.g., per-agent volumes)
        "exomonad-claude-tl",
        "exomonad-claude-pm",
        "exomonad-zellij",
        "exomonad-ssh-keys",
        "exomonad-openobserve",
    ]
    .into_iter()
    .collect();

    let orphan_volumes: Vec<_> = compose_volumes
        .iter()
        .filter(|v| v.starts_with("exomonad-") && !domain_volumes.contains(v.as_str()))
        .collect();

    assert!(
        orphan_volumes.is_empty(),
        "docker-compose.yml has exomonad-* volumes not accounted for: {:?}\n\
         Either add to domain.rs or allowlist in this test",
        orphan_volumes
    );
}
