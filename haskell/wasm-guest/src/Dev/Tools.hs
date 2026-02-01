-- | Dev role tool record.
--
-- Dev agents use Claude Code native tools for most operations.
-- This provides a minimal ping tool to verify WASM guest is responsive.
module Dev.Tools
  ( DevTools (..),
    devToolsHandler,
    devToolsSchema,
  )
where

import GHC.Generics (Generic)

import ExoMonad.Guest.Records.Ping (PingTools (..), pingToolsHandler, pingToolsSchema)
import ExoMonad.Guest.Tool.Mode (AsHandler, AsSchema, ToolMode ((:-)))

-- | Tools available to the Dev role.
--
-- Minimal toolset:
-- - ping: Verify WASM guest is responsive
--
-- All other operations use Claude Code native tools.
data DevTools mode = DevTools
  { util :: PingTools mode
  }
  deriving (Generic)

-- | Dev tools handler record.
devToolsHandler :: DevTools AsHandler
devToolsHandler = DevTools {util = pingToolsHandler}

-- | Dev tools schema record.
devToolsSchema :: DevTools AsSchema
devToolsSchema = DevTools {util = pingToolsSchema}
