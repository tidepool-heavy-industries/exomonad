{-# LANGUAGE OverloadedStrings #-}
module ExoMonad.Guest.Tools.MergePRTests where

import ExoMonad.Guest.Tool.Class (MCPCallOutput (..))
import ExoMonad.Guest.Tools.MergePR

-- Note: These are manual verification tests since a full test runner
-- isn't yet configured for the WASM guest in this environment.

test_render_all_success :: MCPCallOutput
test_render_all_success = mergePRRender $ MergePROutput
  { mpoMerged = True
  , mpoLocalSynced = True
  , mpoCleanupOk = True
  , mpoMessage = "PR #1 merged"
  , mpoBranchName = "main.feat-1"
  , mpoPullError = Nothing
  }
-- Expected: success=true, next says "Verify build"

test_render_merged_but_pull_failed :: MCPCallOutput
test_render_merged_but_pull_failed = mergePRRender $ MergePROutput
  { mpoMerged = True
  , mpoLocalSynced = False
  , mpoCleanupOk = True
  , mpoMessage = "PR #1 merged"
  , mpoBranchName = "main.feat-1"
  , mpoPullError = Just "fatal: some error"
  }
-- Expected: success=false, next says "run git pull --rebase manually"

test_render_merged_pull_ok_cleanup_failed :: MCPCallOutput
test_render_merged_pull_ok_cleanup_failed = mergePRRender $ MergePROutput
  { mpoMerged = True
  , mpoLocalSynced = True
  , mpoCleanupOk = False
  , mpoMessage = "PR #1 merged"
  , mpoBranchName = "main.feat-1"
  , mpoPullError = Nothing
  }
-- Expected: success=true, next mentions manual cleanup

test_render_merge_itself_failed :: MCPCallOutput
test_render_merge_itself_failed = mergePRRender $ MergePROutput
  { mpoMerged = False
  , mpoLocalSynced = True -- shouldn't happen but testing the logic
  , mpoCleanupOk = True
  , mpoMessage = "Merge failed"
  , mpoBranchName = "main.feat-1"
  , mpoPullError = Nothing
  }
-- Expected: success=false, next says "Merge did not complete"

test_render_includes_pull_error_when_set :: MCPCallOutput
test_render_includes_pull_error_when_set = mergePRRender $ MergePROutput
  { mpoMerged = True
  , mpoLocalSynced = False
  , mpoCleanupOk = True
  , mpoMessage = "PR #1 merged"
  , mpoBranchName = "main.feat-1"
  , mpoPullError = Just "fatal: some error"
  }
-- Expected: JSON includes "pull_error": "fatal: some error"
