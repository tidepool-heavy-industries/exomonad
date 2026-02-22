use exomonad_core::services::JjWorkspaceService;
use exomonad_core::domain::BranchName;
use std::fs;
use std::process::Command;
use tempfile::TempDir;
use anyhow::Result;

fn setup_repo() -> Result<TempDir> {
    let temp = TempDir::new()?;
    let path = temp.path();

    // git init
    let status = Command::new("git")
        .arg("init")
        .current_dir(path)
        .status()?;
    assert!(status.success());

    // git config user.email/name (needed for commit)
    Command::new("git")
        .args(["config", "user.email", "test@example.com"])
        .current_dir(path)
        .status()?;
    Command::new("git")
        .args(["config", "user.name", "Test User"])
        .current_dir(path)
        .status()?;

    // create file and commit
    fs::write(path.join("README.md"), "# Test Repo")?;
    let status = Command::new("git")
        .args(["add", "."])
        .current_dir(path)
        .status()?;
    assert!(status.success());

    let status = Command::new("git")
        .args(["commit", "-m", "initial commit"])
        .current_dir(path)
        .status()?;
    assert!(status.success());

    // jj git init --colocate
    let status = Command::new("jj")
        .args(["git", "init", "--colocate"])
        .current_dir(path)
        .status()?;
    assert!(status.success());

    Ok(temp)
}

#[test]
fn test_ensure_colocated() -> Result<()> {
    let temp = TempDir::new()?;
    let path = temp.path();

    // git init
    Command::new("git").arg("init").current_dir(path).status()?;

    // Git needs at least one commit for jj to initialize properly in colocated mode
    fs::write(path.join("README.md"), "# Test Repo")?;
    Command::new("git").args(["add", "."]).current_dir(path).status()?;
    Command::new("git").args(["config", "user.email", "test@example.com"]).current_dir(path).status()?;
    Command::new("git").args(["config", "user.name", "Test User"]).current_dir(path).status()?;
    Command::new("git").args(["commit", "-m", "initial commit"]).current_dir(path).status()?;
    
    let jj = JjWorkspaceService::new(path.to_path_buf());
    jj.ensure_colocated()?;

    assert!(path.join(".jj").exists());
    Ok(())
}

#[test]
fn test_create_workspace() -> Result<()> {
    let temp = setup_repo()?;
    let path = temp.path();
    let jj = JjWorkspaceService::new(path.to_path_buf());

    let ws_path = path.join("child-ws");
    let bookmark = BranchName::from("feature-1");
    
    // Determine default branch name
    let output = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .current_dir(path)
        .output()?;
    let base_name = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let base = BranchName::from(base_name.as_str());

    jj.create_workspace(&ws_path, &bookmark, &base)?;

    assert!(ws_path.exists());
    assert!(ws_path.join(".jj").exists());
    
    // Check if bookmark exists via jj
    let output = Command::new("jj")
        .args(["--no-pager", "bookmark", "list", bookmark.as_str()])
        .current_dir(path)
        .output()?;
    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains(bookmark.as_str()));

    Ok(())
}

#[test]
fn test_remove_workspace() -> Result<()> {
    let temp = setup_repo()?;
    let path = temp.path();
    let jj = JjWorkspaceService::new(path.to_path_buf());

    let ws_path = path.join("child-ws");
    let bookmark = BranchName::from("feature-1");
    
    let output = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .current_dir(path)
        .output()?;
    let base_name = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let base = BranchName::from(base_name.as_str());

    jj.create_workspace(&ws_path, &bookmark, &base)?;
    assert!(ws_path.exists());

    jj.remove_workspace(&ws_path)?;
    assert!(!ws_path.exists());

    Ok(())
}

#[test]
fn test_create_bookmark() -> Result<()> {
    let temp = setup_repo()?;
    let path = temp.path();
    let jj = JjWorkspaceService::new(path.to_path_buf());

    let bookmark = BranchName::from("new-bookmark");
    jj.create_bookmark(path, &bookmark, None)?;

    let output = Command::new("jj")
        .args(["--no-pager", "bookmark", "list", bookmark.as_str()])
        .current_dir(path)
        .output()?;
    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains(bookmark.as_str()));

    Ok(())
}

#[test]
fn test_delete_bookmark() -> Result<()> {
    let temp = setup_repo()?;
    let path = temp.path();
    let jj = JjWorkspaceService::new(path.to_path_buf());

    let bookmark = BranchName::from("to-delete");
    jj.create_bookmark(path, &bookmark, None)?;
    
    jj.delete_bookmark(&bookmark)?;

    let output = Command::new("jj")
        .args(["--no-pager", "bookmark", "list", bookmark.as_str()])
        .current_dir(path)
        .output()?;
    assert!(!String::from_utf8_lossy(&output.stdout).contains(bookmark.as_str()));

    Ok(())
}

#[test]
fn test_get_workspace_bookmark() -> Result<()> {
    let temp = setup_repo()?;
    let path = temp.path();
    let jj = JjWorkspaceService::new(path.to_path_buf());

    let ws_path = path.join("child-ws");
    let bookmark = BranchName::from("feature-x");
    
    let output = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .current_dir(path)
        .output()?;
    let base_name = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let base = BranchName::from(base_name.as_str());

    jj.create_workspace(&ws_path, &bookmark, &base)?;
    
    let found = jj.get_workspace_bookmark(&ws_path)?;
    assert_eq!(found, Some(bookmark.as_str().to_string()));

    Ok(())
}
