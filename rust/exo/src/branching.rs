//! Branch coordinate helpers for the dot-delimited `{parent}.{child}` naming scheme.

use exo_caps::Branch;

/// Everything before the last `.` in the branch name — the parent branch coordinate.
/// Falls back to the full branch string when no `.` is present.
pub(crate) fn parent_branch(branch: &Branch) -> &str {
    branch
        .as_str()
        .rsplit_once('.')
        .map(|(p, _)| p)
        .unwrap_or_else(|| branch.as_str())
}

/// The last `.`-delimited segment of the branch name — the child's own name.
/// Falls back to the full branch string when no `.` is present.
pub(crate) fn child_name(branch: &Branch) -> &str {
    branch
        .as_str()
        .rsplit('.')
        .next()
        .unwrap_or_else(|| branch.as_str())
}

#[cfg(test)]
mod tests {
    use super::*;
    use exo_caps::Branch;

    fn b(s: &str) -> Branch {
        Branch::new(s.to_string()).unwrap()
    }

    #[test]
    fn parent_branch_splits_last_dot() {
        assert_eq!(parent_branch(&b("root.my-tl.my-dev")), "root.my-tl");
        assert_eq!(parent_branch(&b("root.my-dev")), "root");
    }

    #[test]
    fn parent_branch_no_dot_returns_whole() {
        assert_eq!(parent_branch(&b("root")), "root");
    }

    #[test]
    fn child_name_returns_last_segment() {
        assert_eq!(child_name(&b("root.my-tl.my-dev")), "my-dev");
        assert_eq!(child_name(&b("root.my-dev")), "my-dev");
    }

    #[test]
    fn child_name_no_dot_returns_whole() {
        assert_eq!(child_name(&b("root")), "root");
    }
}
