use crate::paths;
use std::io;
use std::path::Path;

/// List all team names in `~/.claude/teams/`.
pub fn list_teams() -> io::Result<Vec<String>> {
    let base = paths::teams_base_dir()
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "HOME directory not found"))?;
    list_teams_at(&base)
}

fn list_teams_at(base: &Path) -> io::Result<Vec<String>> {
    if !base.exists() {
        return Ok(Vec::new());
    }
    let mut teams = Vec::new();
    for entry in std::fs::read_dir(base)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            if let Some(name) = entry.file_name().to_str() {
                teams.push(name.to_string());
            }
        }
    }
    teams.sort();
    Ok(teams)
}

/// List all inbox names (recipients) for a team.
pub fn list_inboxes(team: &str) -> io::Result<Vec<String>> {
    let dir = paths::inbox_dir(team)
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "HOME directory not found"))?;
    list_inboxes_at(&dir)
}

fn list_inboxes_at(dir: &Path) -> io::Result<Vec<String>> {
    if !dir.exists() {
        return Ok(Vec::new());
    }
    let mut names = Vec::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("json") {
            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                if !stem.starts_with('.') {
                    names.push(stem.to_string());
                }
            }
        }
    }
    names.sort();
    Ok(names)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_list_teams_empty() {
        let tmp = tempdir().unwrap();
        let result = list_teams_at(tmp.path()).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn test_list_teams() {
        let tmp = tempdir().unwrap();
        std::fs::create_dir(tmp.path().join("alpha")).unwrap();
        std::fs::create_dir(tmp.path().join("beta")).unwrap();
        std::fs::write(tmp.path().join("not-a-dir.txt"), "").unwrap();

        let result = list_teams_at(tmp.path()).unwrap();
        assert_eq!(result, vec!["alpha", "beta"]);
    }

    #[test]
    fn test_list_inboxes() {
        let tmp = tempdir().unwrap();
        std::fs::write(tmp.path().join("alice.json"), "[]").unwrap();
        std::fs::write(tmp.path().join("bob.json"), "[]").unwrap();
        std::fs::write(tmp.path().join(".tmp.json"), "[]").unwrap();
        std::fs::write(tmp.path().join("readme.txt"), "").unwrap();

        let result = list_inboxes_at(tmp.path()).unwrap();
        assert_eq!(result, vec!["alice", "bob"]);
    }

    #[test]
    fn test_list_nonexistent() {
        let result = list_teams_at(Path::new("/nonexistent/path")).unwrap();
        assert!(result.is_empty());
    }
}
