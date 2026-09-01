use std::path::PathBuf;
use std::process::Command;

fn git_output(args: &[&str]) -> Option<String> {
    Command::new("git")
        .args(args)
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        .filter(|value| !value.is_empty())
}

fn watch_git_path(pathspec: &str) {
    if let Some(path) = git_output(&["rev-parse", "--git-path", pathspec]) {
        println!("cargo:rerun-if-changed={}", PathBuf::from(path).display());
    }
}

fn main() {
    println!("cargo:rerun-if-changed=src");
    watch_git_path("HEAD");
    watch_git_path("packed-refs");

    // A normal commit updates the symbolic branch ref, not .git/HEAD. `--git-path`
    // resolves both ordinary repositories and linked-worktree/common-dir layouts.
    if let Some(head_ref) = git_output(&["symbolic-ref", "-q", "HEAD"]) {
        watch_git_path(&head_ref);
    }

    let revision = git_output(&["describe", "--always", "--dirty=-dirty"])
        .unwrap_or_else(|| "unknown".to_owned());
    println!("cargo:rustc-env=EXO_BUILD_REVISION={revision}");
}
