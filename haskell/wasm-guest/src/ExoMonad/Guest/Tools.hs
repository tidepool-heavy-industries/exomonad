{-# LANGUAGE OverloadedStrings #-}

-- | MCP Tool Definitions (Single Source of Truth)
--
-- All tool schemas are defined here and exported to Rust via WASM.
-- This eliminates the schema/handler mismatch risk.
module ExoMonad.Guest.Tools
  ( ToolDefinition (..),
    allTools,
    toMCPFormat,
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)

-- | Tool definition for MCP discovery.
data ToolDefinition = ToolDefinition
  { tdName :: Text,
    tdDescription :: Text,
    tdInputSchema :: Value
  }

-- | Convert a tool definition to MCP JSON format.
toMCPFormat :: ToolDefinition -> Value
toMCPFormat t =
  object
    [ "name" .= tdName t,
      "description" .= tdDescription t,
      "inputSchema" .= tdInputSchema t
    ]

-- | All available MCP tools.
allTools :: [ToolDefinition]
allTools =
  [ gitBranch,
    gitStatus,
    gitLog,
    readFileTool,
    githubListIssues,
    githubGetIssue,
    githubListPRs,
    spawnAgents,
    cleanupAgents,
    listAgents
  ]

-- ============================================================================
-- Git Tools
-- ============================================================================

gitBranch :: ToolDefinition
gitBranch =
  ToolDefinition
    { tdName = "git_branch",
      tdDescription = "Get the current git branch name",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Directory path (defaults to project root)" :: Text)
                      ]
                ]
          ]
    }

gitStatus :: ToolDefinition
gitStatus =
  ToolDefinition
    { tdName = "git_status",
      tdDescription = "Get list of modified/untracked files (git status --porcelain)",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Directory path (defaults to project root)" :: Text)
                      ]
                ]
          ]
    }

gitLog :: ToolDefinition
gitLog =
  ToolDefinition
    { tdName = "git_log",
      tdDescription = "Get recent git commits",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Directory path (defaults to project root)" :: Text)
                      ],
                  "limit"
                    .= object
                      [ "type" .= ("integer" :: Text),
                        "description" .= ("Number of commits to show (default: 10)" :: Text),
                        "default" .= (10 :: Int)
                      ]
                ]
          ]
    }

-- ============================================================================
-- File Tools
-- ============================================================================

readFileTool :: ToolDefinition
readFileTool =
  ToolDefinition
    { tdName = "read_file",
      tdDescription = "Read contents of a file",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "required" .= (["path"] :: [Text]),
            "properties"
              .= object
                [ "path"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("File path (relative to project root or absolute)" :: Text)
                      ],
                  "max_lines"
                    .= object
                      [ "type" .= ("integer" :: Text),
                        "description" .= ("Maximum lines to read (default: unlimited)" :: Text)
                      ]
                ]
          ]
    }

-- ============================================================================
-- GitHub Tools
-- ============================================================================

githubListIssues :: ToolDefinition
githubListIssues =
  ToolDefinition
    { tdName = "github_list_issues",
      tdDescription = "List issues from a GitHub repository",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "required" .= (["owner", "repo"] :: [Text]),
            "properties"
              .= object
                [ "owner"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Repository owner (user or org)" :: Text)
                      ],
                  "repo"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Repository name" :: Text)
                      ],
                  "state"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "enum" .= (["open", "closed", "all"] :: [Text]),
                        "description" .= ("Filter by issue state (default: open)" :: Text)
                      ],
                  "labels"
                    .= object
                      [ "type" .= ("array" :: Text),
                        "items" .= object ["type" .= ("string" :: Text)],
                        "description" .= ("Filter by labels" :: Text)
                      ]
                ]
          ]
    }

githubGetIssue :: ToolDefinition
githubGetIssue =
  ToolDefinition
    { tdName = "github_get_issue",
      tdDescription = "Get a single GitHub issue with full details",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "required" .= (["owner", "repo", "number"] :: [Text]),
            "properties"
              .= object
                [ "owner"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Repository owner (user or org)" :: Text)
                      ],
                  "repo"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Repository name" :: Text)
                      ],
                  "number"
                    .= object
                      [ "type" .= ("integer" :: Text),
                        "description" .= ("Issue number" :: Text)
                      ]
                ]
          ]
    }

githubListPRs :: ToolDefinition
githubListPRs =
  ToolDefinition
    { tdName = "github_list_prs",
      tdDescription = "List pull requests from a GitHub repository",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "required" .= (["owner", "repo"] :: [Text]),
            "properties"
              .= object
                [ "owner"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Repository owner (user or org)" :: Text)
                      ],
                  "repo"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Repository name" :: Text)
                      ],
                  "state"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "enum" .= (["open", "closed", "all"] :: [Text]),
                        "description" .= ("Filter by PR state (default: open)" :: Text)
                      ],
                  "limit"
                    .= object
                      [ "type" .= ("integer" :: Text),
                        "description" .= ("Maximum number of PRs to return" :: Text)
                      ]
                ]
          ]
    }

-- ============================================================================
-- Agent Control Tools
-- ============================================================================

spawnAgents :: ToolDefinition
spawnAgents =
  ToolDefinition
    { tdName = "spawn_agents",
      tdDescription = "Spawn Claude Code agents for GitHub issues in isolated worktrees",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "required" .= (["issues", "owner", "repo"] :: [Text]),
            "properties"
              .= object
                [ "issues"
                    .= object
                      [ "type" .= ("array" :: Text),
                        "items" .= object ["type" .= ("string" :: Text)],
                        "description" .= ("GitHub issue numbers to spawn agents for" :: Text)
                      ],
                  "owner"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("GitHub repository owner" :: Text)
                      ],
                  "repo"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("GitHub repository name" :: Text)
                      ],
                  "worktree_dir"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Base directory for worktrees (default: ./worktrees)" :: Text)
                      ]
                ]
          ]
    }

cleanupAgents :: ToolDefinition
cleanupAgents =
  ToolDefinition
    { tdName = "cleanup_agents",
      tdDescription = "Clean up agent worktrees and close their Zellij tabs",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "required" .= (["issues"] :: [Text]),
            "properties"
              .= object
                [ "issues"
                    .= object
                      [ "type" .= ("array" :: Text),
                        "items" .= object ["type" .= ("string" :: Text)],
                        "description" .= ("Issue IDs to clean up" :: Text)
                      ],
                  "force"
                    .= object
                      [ "type" .= ("boolean" :: Text),
                        "description" .= ("Force deletion even if worktree has uncommitted changes (default: false)" :: Text)
                      ]
                ]
          ]
    }

listAgents :: ToolDefinition
listAgents =
  ToolDefinition
    { tdName = "list_agents",
      tdDescription = "List active agent worktrees",
      tdInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties" .= object []
          ]
    }
