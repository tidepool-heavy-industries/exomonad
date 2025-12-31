-- | Telegram effect for Tidepool agents
--
-- This module provides a typed DSL for Telegram bot interactions,
-- designed for use with the Tidepool agent framework.
--
-- == Overview
--
-- The Telegram effect uses a Send/Receive model:
--
-- * 'tgSend' - Fire and forget message sending
-- * 'tgReceive' - Block until messages arrive (returns 'NonEmpty')
-- * 'tgTryReceive' - Non-blocking check (may return empty)
--
-- == Example
--
-- @
-- import Tidepool.Telegram
--
-- greetUser :: Telegram :> es => Eff es Text
-- greetUser = do
--   responses <- askText "What's your name?"
--   case firstText responses of
--     Just name -> do
--       tgSend (TextMsg $ "Hello, " <> name <> "!")
--       pure name
--     Nothing -> greetUser  -- ask again if they sent media instead
-- @
--
-- == Design Notes
--
-- * __Thread-blind__: Each graph instance owns one conversation.
--   The runtime routes messages to the correct instance.
--
-- * __Accumulating receive__: 'tgReceive' returns ALL pending messages,
--   not just one. This handles the case where users send multiple
--   messages before the bot responds.
--
-- * __Separate message types__: 'IncomingMessage' distinguishes text,
--   photos, documents, and button clicks for clean pattern matching.
module Tidepool.Telegram
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

    -- * Types
  , MediaHandle(..)
  , InlineButton(..)
  , OutgoingMessage(..)
  , IncomingMessage(..)

    -- * Stub Runner
  , runTelegramStub
  ) where

import Tidepool.Telegram.Effect
import Tidepool.Telegram.Types
