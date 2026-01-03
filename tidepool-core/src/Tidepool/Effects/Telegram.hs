-- | Telegram messaging effect with type-safe buttons and responses
module Tidepool.Effects.Telegram
  ( -- * Effect
    Telegram(..)
  , telegramSend
  , telegramMarkdown
  , telegramHtml
  , telegramAsk

    -- * Try Variants (return Either instead of crashing)
  , telegramSendTry
  , telegramMarkdownTry
  , telegramHtmlTry
  , telegramAskTry

    -- * Error Types
  , TelegramError(..)

    -- * Types
  , MessageId(..)
  , CallbackData(..)
  , ParseMode(..)
  , TelegramMessage(..)
  , TelegramButton(..)

    -- * Helpers
  , parseModeText

    -- * Runner (stub)
  , runTelegramStub
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send, interpret)

import Tidepool.Effect (Log, logInfo)

-- Types

-- | Message ID returned by Telegram API, useful for editing/deleting
newtype MessageId = MessageId { unMessageId :: Int }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Callback data from button press
newtype CallbackData = CallbackData { unCallbackData :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, IsString)

-- | Parse mode for message formatting
data ParseMode = PlainText | Markdown | HTML
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Structured message with text and formatting
data TelegramMessage = TelegramMessage
  { tmText :: Text
  , tmParseMode :: ParseMode
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Button definition for inline keyboards
data TelegramButton = TelegramButton
  { tbLabel :: Text           -- ^ What user sees
  , tbCallback :: CallbackData  -- ^ What we receive on click
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Structured error type for Telegram API failures
data TelegramError
  = TelegramNetworkError Text   -- ^ Network/connection failure
  | TelegramRateLimited         -- ^ Rate limit hit
  | TelegramInvalidChat Text    -- ^ Chat not found or bot kicked
  | TelegramUnauthorized        -- ^ Invalid bot token
  | TelegramTimeout             -- ^ Request timed out (for ask operations)
  | TelegramOther Text          -- ^ Other errors with message
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Effect

-- | Telegram messaging effect
data Telegram r where
  -- | Send a message, returns message ID for potential editing
  TelegramSendOp :: TelegramMessage -> Telegram MessageId

  -- | Send message with inline keyboard, blocks until user clicks a button
  TelegramAskOp :: TelegramMessage -> [TelegramButton] -> Telegram CallbackData

  -- | Try variants (return Either instead of crashing)
  TelegramSendTryOp :: TelegramMessage -> Telegram (Either TelegramError MessageId)
  TelegramAskTryOp :: TelegramMessage -> [TelegramButton] -> Telegram (Either TelegramError CallbackData)

-- | Send plain text message
telegramSend :: Member Telegram effs => Text -> Eff effs MessageId
telegramSend txt = send $ TelegramSendOp (TelegramMessage txt PlainText)

-- | Send Markdown-formatted message
telegramMarkdown :: Member Telegram effs => Text -> Eff effs MessageId
telegramMarkdown txt = send $ TelegramSendOp (TelegramMessage txt Markdown)

-- | Send HTML-formatted message
telegramHtml :: Member Telegram effs => Text -> Eff effs MessageId
telegramHtml txt = send $ TelegramSendOp (TelegramMessage txt HTML)

-- | Ask with custom buttons, returns the callback data as Text
telegramAsk :: Member Telegram effs
            => Text
            -> [(Text, Text)]  -- ^ [(label, callback)]
            -> Eff effs Text
telegramAsk msg buttons = do
  CallbackData cb <- send $ TelegramAskOp
    (TelegramMessage msg Markdown)
    [TelegramButton lbl (CallbackData cb') | (lbl, cb') <- buttons]
  pure cb

-- | Try variants: return Either instead of crashing

-- | Send plain text message (try variant)
telegramSendTry :: Member Telegram effs => Text -> Eff effs (Either TelegramError MessageId)
telegramSendTry txt = send $ TelegramSendTryOp (TelegramMessage txt PlainText)

-- | Send Markdown-formatted message (try variant)
telegramMarkdownTry :: Member Telegram effs => Text -> Eff effs (Either TelegramError MessageId)
telegramMarkdownTry txt = send $ TelegramSendTryOp (TelegramMessage txt Markdown)

-- | Send HTML-formatted message (try variant)
telegramHtmlTry :: Member Telegram effs => Text -> Eff effs (Either TelegramError MessageId)
telegramHtmlTry txt = send $ TelegramSendTryOp (TelegramMessage txt HTML)

-- | Ask with custom buttons (try variant)
telegramAskTry :: Member Telegram effs
               => Text
               -> [(Text, Text)]  -- ^ [(label, callback)]
               -> Eff effs (Either TelegramError Text)
telegramAskTry msg buttons = do
  result <- send $ TelegramAskTryOp
    (TelegramMessage msg Markdown)
    [TelegramButton lbl (CallbackData cb') | (lbl, cb') <- buttons]
  pure $ fmap (\(CallbackData cb) -> cb) result
-- Helpers

-- | Convert ParseMode to text for serialization
parseModeText :: ParseMode -> Text
parseModeText PlainText = "PlainText"
parseModeText Markdown = "Markdown"
parseModeText HTML = "HTML"

-- Stub runner (errors on call)

runTelegramStub :: Member Log effs => Eff (Telegram ': effs) a -> Eff effs a
runTelegramStub = interpret $ \case
  -- Original operations (crash on call)
  TelegramSendOp msg -> do
    logInfo $ "[Telegram:stub] TelegramSend called: " <> msg.tmText
    error "Telegram.telegramSend: not implemented"
  TelegramAskOp msg buttons -> do
    logInfo $ "[Telegram:stub] TelegramAsk called: " <> msg.tmText <> " with " <> showButtons buttons
    error "Telegram.telegramAsk: not implemented"

  -- Try variants (return Left with error instead of crashing)
  TelegramSendTryOp msg -> do
    logInfo $ "[Telegram:stub] TelegramSendTry called: " <> msg.tmText
    pure $ Left (TelegramOther "Telegram.telegramSend: not implemented (stub)")
  TelegramAskTryOp msg buttons -> do
    logInfo $ "[Telegram:stub] TelegramAskTry called: " <> msg.tmText <> " with " <> showButtons buttons
    pure $ Left (TelegramOther "Telegram.telegramAsk: not implemented (stub)")

showButtons :: [TelegramButton] -> Text
showButtons bs = "[" <> mconcat [b.tbLabel <> "," | b <- bs] <> "]"
