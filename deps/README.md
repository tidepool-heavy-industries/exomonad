# Vendored Dependencies

Local copies of forked dependencies to avoid `source-repository-package` git issues in worktrees.

## freer-simple

**Source**: https://github.com/georgefst/freer-simple
**Commit**: `e1d88c1ee036115ef527bda8c66da997962b3f34`
**Why**: GHC 9.10/9.12 compatibility ([PR #45](https://github.com/lexi-lambda/freer-simple/pull/45), not yet merged upstream)

Last upstream release (1.2.1.2) was January 2022 and doesn't support GHC 9.10+.

## ginger

**Source**: https://github.com/inanna-malick/ginger
**Commit**: `25af8c8e60f4afbfdb4e50e6d1d86a5e73975638`
**Why**: Typed templates with compile-time validation + dependency tracking (tree API)

Fork of the ginger templating library with tidepool-specific enhancements.

## Updating

To update a vendored dependency:

```bash
cd deps/<package>
git init
git remote add origin <upstream-url>
git fetch origin
git checkout <new-commit>
rm -rf .git .github
```

Then update the commit reference in this README.
