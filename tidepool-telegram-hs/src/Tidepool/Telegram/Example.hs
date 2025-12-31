{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Example Telegram graph: Echo Bot
--
-- A simple logic-only graph demonstrating the Telegram effect DSL.
-- No LLM nodes - pure routing logic based on commands.
--
-- Graph structure:
--
-- @
-- Entry (ChatId)
--    │
--    ▼
-- ┌──────────────────────────┐
-- │  mainLoop (LogicNode)    │◄──────────────┐
-- │  - tgSend welcome        │               │
-- │  - tgReceive             │               │
-- │  - Parse command         │               │
-- │  - Route based on input  │               │
-- └──────────┬───────────────┘               │
--            │                               │
--      ┌─────┴─────┬────────────┐           │
--      ▼           ▼            ▼           │
--  "/help"     "/quit"     other text       │
--      │           │            │           │
--      ▼           ▼            ▼           │
--  send help   send bye     echo back       │
--      │           │            │           │
--      │           ▼            └───────────┘
--      │        Exit (Unit)
--      └────────────────────────────────────┘
-- @
module Tidepool.Telegram.Example
  ( -- * Graph Definition
    EchoGraph(..)
  , echoHandlers

    -- * Types
  , ChatId(..)

    -- * Validation
  , validEchoGraph
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import GHC.Generics (Generic)

import Tidepool.Graph.Types (Needs, UsesEffects, Exit, type (:@))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto, To, GotoChoice, gotoChoice, gotoExit)
import Tidepool.Telegram.Effect (Telegram, tgSend, tgReceive)
import Tidepool.Telegram.Types (OutgoingMessage(..), IncomingMessage(..))

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Chat identifier (opaque to graph, interpreted by runtime)
newtype ChatId = ChatId Int
  deriving stock (Show, Eq, Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Echo bot graph - pure logic, no LLM nodes.
--
-- Demonstrates:
--
-- * Using 'Telegram' effect for send/receive
-- * Self-loop via @Goto "egMainLoop"@
-- * Exit via @Goto Exit ()@
-- * Pattern matching on 'IncomingMessage'
data EchoGraph mode = EchoGraph
  { egEntry    :: mode :- G.Entry ChatId
  , egMainLoop :: mode :- G.LogicNode
                    :@ Needs '[ChatId]
                    :@ UsesEffects '[Goto "egMainLoop" ChatId, Goto Exit ()]
  , egExit     :: mode :- G.Exit ()
  }
  deriving Generic

-- | Handler record for EchoGraph.
--
-- The 'Telegram' effect is available in handlers because it's in the
-- 'AsHandler' mode's effect stack.
echoHandlers :: EchoGraph (AsHandler '[Telegram])
echoHandlers = EchoGraph
  { egEntry    = Proxy @ChatId
  , egMainLoop = mainLoopHandler
  , egExit     = Proxy @()
  }

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Main loop handler: send welcome, receive, route, repeat or exit.
--
-- Note: This sends a welcome message on every loop iteration. A real bot
-- would track state to only send it once. This keeps the example simple.
mainLoopHandler
  :: Telegram :> es
  => ChatId
  -> Eff es (GotoChoice '[To "egMainLoop" ChatId, To Exit ()])
mainLoopHandler chatId = do
  -- Send welcome prompt
  tgSend (TextMsg "Echo bot ready! Commands: /help, /quit, or type anything")

  -- Block until user sends something
  messages <- tgReceive

  -- Process first message (explicit pattern matching for all IncomingMessage types)
  case messages of
    (TextReply text :| _) -> handleTextCommand chatId text
    (PhotoReply _ _ :| _) -> do
      tgSend (TextMsg "Photo messages not supported (text only)")
      pure $ gotoChoice @"egMainLoop" chatId
    (DocReply _ _ :| _)   -> do
      tgSend (TextMsg "Document messages not supported (text only)")
      pure $ gotoChoice @"egMainLoop" chatId
    (ButtonClick _ :| _)  -> do
      tgSend (TextMsg "Button clicks not supported in echo bot")
      pure $ gotoChoice @"egMainLoop" chatId

-- | Route based on text command.
handleTextCommand
  :: Telegram :> es
  => ChatId
  -> Text
  -> Eff es (GotoChoice '[To "egMainLoop" ChatId, To Exit ()])
handleTextCommand chatId text
  | text == "/quit" = do
      tgSend (TextMsg "Goodbye!")
      pure $ gotoExit ()
  | text == "/help" = do
      tgSend (TextMsg helpText)
      pure $ gotoChoice @"egMainLoop" chatId
  | echoPrefix `T.isPrefixOf` text = do
      let msg = T.drop (T.length echoPrefix) text
      tgSend (TextMsg msg)
      pure $ gotoChoice @"egMainLoop" chatId
  | otherwise = do
      tgSend (TextMsg $ "You said: " <> text)
      pure $ gotoChoice @"egMainLoop" chatId

-- | Command prefix for echo command.
echoPrefix :: Text
echoPrefix = "/echo "

-- | Help text for the echo bot.
helpText :: Text
helpText = T.unlines
  [ "Echo Bot Commands:"
  , "/help  - Show this message"
  , "/echo <text> - Echo text back"
  , "/quit  - End conversation"
  , "Or just type anything to have it echoed!"
  ]

-- ════════════════════════════════════════════════════════════════════════════
-- VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile-time validation of EchoGraph structure.
--
-- This uses ValidGraphRecord to verify at compile time that:
--
-- * EchoGraph has an Entry field
-- * EchoGraph has an Exit field
-- * All Goto targets reference valid field names
--
-- If validation fails, you get a compile-time error.
validEchoGraph :: G.ValidGraphRecord EchoGraph => ()
validEchoGraph = ()
