//! Authoritative role-protocol resolution and launch provenance.

use sha2::{Digest, Sha256};
use std::fmt;
use std::path::{Path, PathBuf};

pub const BUILD_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_REVISION: &str = env!("EXO_BUILD_REVISION");

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProtocolSource {
    Baked,
    Override(PathBuf),
}

impl fmt::Display for ProtocolSource {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Baked => formatter.write_str("baked"),
            Self::Override(path) => write!(formatter, "override:{}", path.display()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedProtocol {
    pub text: String,
    pub source: ProtocolSource,
    pub sha256: String,
}

impl ResolvedProtocol {
    fn new(text: String, source: ProtocolSource) -> Self {
        let sha256 = format!("{:x}", Sha256::digest(text.as_bytes()));
        Self {
            text,
            source,
            sha256,
        }
    }

    pub fn short_hash(&self) -> &str {
        &self.sha256[..12]
    }
}

/// Resolve the effective role charter for both roots and spawned nodes. A tracked on-disk override
/// supports prompt tuning; absence, read failure, or invalid UTF-8 falls back loudly to the baked
/// charter so launch never depends on an unreadable optional file.
pub async fn resolve_role_protocol(
    working_dir: &Path,
    role: &str,
    baked: &str,
) -> ResolvedProtocol {
    let path = working_dir.join(format!(".exo/roles/devswarm/context/{role}.md"));
    match tokio::fs::read(&path).await {
        Ok(bytes) => match String::from_utf8(bytes) {
            Ok(text) => ResolvedProtocol::new(text, ProtocolSource::Override(path)),
            Err(error) => {
                tracing::warn!(
                    path = %path.display(),
                    %error,
                    "role protocol override is not UTF-8; using baked charter"
                );
                ResolvedProtocol::new(baked.to_owned(), ProtocolSource::Baked)
            }
        },
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            ResolvedProtocol::new(baked.to_owned(), ProtocolSource::Baked)
        }
        Err(error) => {
            tracing::warn!(
                path = %path.display(),
                %error,
                "role protocol override is unreadable; using baked charter"
            );
            ResolvedProtocol::new(baked.to_owned(), ProtocolSource::Baked)
        }
    }
}

pub fn log_launch_provenance(role: &str, protocol: &ResolvedProtocol) {
    tracing::info!(
        binary_version = BUILD_VERSION,
        binary_revision = BUILD_REVISION,
        role,
        protocol_source = %protocol.source,
        protocol_sha256 = protocol.short_hash(),
        "launch prompt provenance"
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn resolves_baked_protocol_with_stable_hash_when_override_is_absent() {
        let dir = tempfile::tempdir().unwrap();
        let resolved = resolve_role_protocol(dir.path(), "dev", "baked charter").await;
        assert_eq!(resolved.text, "baked charter");
        assert_eq!(resolved.source, ProtocolSource::Baked);
        assert_eq!(resolved.sha256.len(), 64);
        assert_eq!(resolved.short_hash().len(), 12);
    }

    #[tokio::test]
    async fn resolves_override_and_reports_its_path() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(".exo/roles/devswarm/context/tl.md");
        tokio::fs::create_dir_all(path.parent().unwrap())
            .await
            .unwrap();
        tokio::fs::write(&path, "override charter").await.unwrap();

        let resolved = resolve_role_protocol(dir.path(), "tl", "baked charter").await;
        assert_eq!(resolved.text, "override charter");
        assert_eq!(resolved.source, ProtocolSource::Override(path));
        assert_ne!(
            resolved.sha256,
            ResolvedProtocol::new("baked charter".into(), ProtocolSource::Baked).sha256
        );
    }
}
