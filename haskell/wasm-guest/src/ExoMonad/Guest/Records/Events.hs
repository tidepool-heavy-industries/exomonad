-- | Events core re-exports for role code.
module ExoMonad.Guest.Records.Events
  ( -- * SendMessage (MCPTool instance stays in SDK)
    SendMessage,

    -- * NotifyParent (core + shared schema, no MCPTool instance)
    NotifyParent,
    NotifyParentArgs (..),
    notifyParentCore,
    notifyParentDescription,
    notifyParentSchema,
    NotifyStatus (..),
    TaskReport (..),

    -- * Shutdown (core + shared schema, no MCPTool instance)
    Shutdown,
    ShutdownArgs (..),
    shutdownCore,
    shutdownDescription,
    shutdownSchema,
  )
where

import ExoMonad.Guest.Tools.Events
