-- | Dev role tool record.
--
-- Dev agents use Claude Code native tools for most operations.
-- This provides tools for WASM guest health checks and PR operations.
module Dev.Tools
  ( DevTools (..),
    devToolsHandler,
    devToolsSchema,
  )
where

import ExoMonad.Guest.Records.FilePR (FilePRTools (..), filePRToolsHandler, filePRToolsSchema)
import ExoMonad.Guest.Records.Ping (PingTools (..), pingToolsHandler, pingToolsSchema)
import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema)
import GHC.Generics (Generic)

-- | Tools available to the Dev role.
--
-- Toolset:
-- - ping: Verify WASM guest is responsive
-- - file_pr: Create or update PRs for the current branch
--
-- All other operations use Claude Code native tools.
data DevTools mode = DevTools
  { util :: PingTools mode,
    pr :: FilePRTools mode
  }
  deriving (Generic)

-- | Dev tools handler record.
devToolsHandler :: DevTools AsHandler
devToolsHandler =
  DevTools
    { util = pingToolsHandler,
      pr = filePRToolsHandler
    }

-- | Dev tools schema record.
devToolsSchema :: DevTools AsSchema
devToolsSchema =
  DevTools
    { util = pingToolsSchema,
      pr = filePRToolsSchema
    }
