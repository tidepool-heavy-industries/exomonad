-- | Telegram effect for conversational agents
--
-- This module provides a Send/Receive model for Telegram interactions:
--
-- * 'tgSend' - Fire and forget message sending
-- * 'tgReceive' - Block until at least one message arrives
-- * 'tgTryReceive' - Non-blocking check for pending messages
--
-- The 'ask' combinator combines send + receive for common request-response flows.
module Tidepool.Telegram.Effect
  ( -- * Effect
    Telegram(..)

    -- * Operations
  , tgSend
  , tgReceive
  , tgTryReceive

    -- * Combinators
  , ask
  , askText
  , askButtons

    -- * Extractors
  , firstText
  , firstClick
  , firstMedia
  , allTexts
  , allClicks
  , allMedia

    -- * Stub Runner
  , runTelegramStub
  ) where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic

import Tidepool.Telegram.Types

-- | The Telegram effect for conversational messaging.
--
-- Thread-blind: each graph instance "owns" one conversation thread.
-- The runtime handles routing messages to the correct graph instance.
data Telegram :: Effect where
  -- | Send a message (fire and forget)
  Send :: OutgoingMessage -> Telegram m ()

  -- | Block until at least one message arrives, return all pending
  Receive :: Telegram m (NonEmpty IncomingMessage)

  -- | Non-blocking check for pending messages (may be empty)
  TryReceive :: Telegram m [IncomingMessage]

type instance DispatchOf Telegram = 'Dynamic

-- ══════════════════════════════════════════════════════════════
-- OPERATIONS
-- ══════════════════════════════════════════════════════════════

-- | Send an outgoing message (fire and forget).
tgSend :: Telegram :> es => OutgoingMessage -> Eff es ()
tgSend = send . Send

-- | Block until at least one incoming message arrives.
-- Returns all pending messages (guaranteed non-empty).
tgReceive :: Telegram :> es => Eff es (NonEmpty IncomingMessage)
tgReceive = send Receive

-- | Non-blocking check for pending messages.
-- Returns empty list if no messages are pending.
tgTryReceive :: Telegram :> es => Eff es [IncomingMessage]
tgTryReceive = send TryReceive

-- ══════════════════════════════════════════════════════════════
-- COMBINATORS
-- ══════════════════════════════════════════════════════════════

-- | Send a message and wait for response(s).
--
-- This is the common request-response pattern: send something,
-- then block until the user replies.
ask :: Telegram :> es => OutgoingMessage -> Eff es (NonEmpty IncomingMessage)
ask msg = tgSend msg >> tgReceive

-- | Send a text message and wait for response(s).
askText :: Telegram :> es => Text -> Eff es (NonEmpty IncomingMessage)
askText = ask . TextMsg

-- | Send a message with buttons and wait for response(s).
--
-- Note: The user might reply with text instead of clicking a button.
-- Use 'firstClick' to extract button responses.
askButtons :: Telegram :> es => Text -> [[InlineButton]] -> Eff es (NonEmpty IncomingMessage)
askButtons text buttons = ask (ButtonsMsg text buttons)

-- ══════════════════════════════════════════════════════════════
-- EXTRACTORS
-- ══════════════════════════════════════════════════════════════

-- | Extract the first text reply from messages.
firstText :: NonEmpty IncomingMessage -> Maybe Text
firstText = listToMaybe . allTexts

-- | Extract the first button click from messages.
firstClick :: NonEmpty IncomingMessage -> Maybe Value
firstClick = listToMaybe . allClicks

-- | Extract the first media handle from messages.
firstMedia :: NonEmpty IncomingMessage -> Maybe MediaHandle
firstMedia = listToMaybe . allMedia

-- | Extract all text replies from messages.
allTexts :: NonEmpty IncomingMessage -> [Text]
allTexts = mapMaybe getText . NE.toList
  where
    getText (TextReply t) = Just t
    getText _ = Nothing

-- | Extract all button clicks from messages.
allClicks :: NonEmpty IncomingMessage -> [Value]
allClicks = mapMaybe getClick . NE.toList
  where
    getClick (ButtonClick v) = Just v
    getClick _ = Nothing

-- | Extract all media handles from messages.
allMedia :: NonEmpty IncomingMessage -> [MediaHandle]
allMedia = mapMaybe getMedia . NE.toList
  where
    getMedia (PhotoReply h _) = Just h
    getMedia (DocReply h _) = Just h
    getMedia _ = Nothing

-- ══════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ══════════════════════════════════════════════════════════════

-- | Stub interpreter that errors on any operation.
--
-- Useful for testing graph structure without a real Telegram connection.
runTelegramStub :: Eff (Telegram : es) a -> Eff es a
runTelegramStub = interpret $ \_ -> \case
  Send _msg -> error "Telegram.Send: stub - not implemented"
  Receive -> error "Telegram.Receive: stub - not implemented"
  TryReceive -> error "Telegram.TryReceive: stub - not implemented"
