-- | Telegram messaging effect with type-safe buttons and responses
module Tidepool.Effects.Telegram
  ( -- * Effect
    Telegram(..)
  , telegramSend
  , telegramMarkdown
  , telegramHtml
  , telegramAsk

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
import Effectful
import Effectful.Dispatch.Dynamic

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

-- Effect

-- | Telegram messaging effect
data Telegram :: Effect where
  -- | Send a message, returns message ID for potential editing
  TelegramSendOp :: TelegramMessage -> Telegram m MessageId

  -- | Send message with inline keyboard, blocks until user clicks a button
  TelegramAskOp :: TelegramMessage -> [TelegramButton] -> Telegram m CallbackData

type instance DispatchOf Telegram = 'Dynamic

-- | Send plain text message
telegramSend :: Telegram :> es => Text -> Eff es MessageId
telegramSend txt = send $ TelegramSendOp (TelegramMessage txt PlainText)

-- | Send Markdown-formatted message
telegramMarkdown :: Telegram :> es => Text -> Eff es MessageId
telegramMarkdown txt = send $ TelegramSendOp (TelegramMessage txt Markdown)

-- | Send HTML-formatted message
telegramHtml :: Telegram :> es => Text -> Eff es MessageId
telegramHtml txt = send $ TelegramSendOp (TelegramMessage txt HTML)

-- | Ask with custom buttons, returns the callback data as Text
telegramAsk :: Telegram :> es
            => Text
            -> [(Text, Text)]  -- ^ [(label, callback)]
            -> Eff es Text
telegramAsk msg buttons = do
  CallbackData cb <- send $ TelegramAskOp
    (TelegramMessage msg Markdown)
    [TelegramButton lbl (CallbackData cb') | (lbl, cb') <- buttons]
  pure cb


-- Helpers

-- | Convert ParseMode to text for serialization
parseModeText :: ParseMode -> Text
parseModeText PlainText = "PlainText"
parseModeText Markdown = "Markdown"
parseModeText HTML = "HTML"

-- Stub runner (errors on call)

runTelegramStub :: (IOE :> es, Log :> es) => Eff (Telegram : es) a -> Eff es a
runTelegramStub = interpret $ \_ -> \case
  TelegramSendOp msg -> do
    logInfo $ "[Telegram:stub] TelegramSend called: " <> msg.tmText
    error "Telegram.telegramSend: not implemented"
  TelegramAskOp msg buttons -> do
    logInfo $ "[Telegram:stub] TelegramAsk called: " <> msg.tmText <> " with " <> showButtons buttons
    error "Telegram.telegramAsk: not implemented"

showButtons :: [TelegramButton] -> Text
showButtons bs = "[" <> mconcat [b.tbLabel <> "," | b <- bs] <> "]"
