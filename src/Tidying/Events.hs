{-# LANGUAGE OverloadedStrings #-}
-- | Tidying Events
--
-- Events emitted during tidying sessions.
-- Separate module to avoid import cycles between Loop and Tools.
module Tidying.Events
  ( TidyingEvent(..)
  ) where

import Data.Text (Text)
import Tidying.State (Mode)

-- | Events emitted during tidying
data TidyingEvent
  = PhotoAnalyzed Text          -- ^ Photo was analyzed
  | SituationClassified Text    -- ^ Situation was classified (deprecated, will be removed)
  | ModeChanged Mode Mode       -- ^ Mode transition (from, to)
  | SessionEnded Int            -- ^ Session ended, items processed
  | UserInputReceived Text      -- ^ User input received (for chat display)
  | PhotoUploaded Text Text     -- ^ Photo uploaded: base64, mimeType
  | ResponseGenerated Text      -- ^ Response generated (for chat display)
  | ErrorOccurred Text          -- ^ Error occurred (for visible feedback)
  -- Tool events (from mid-turn tool calls)
  | ItemProposed Text [Text]    -- ^ Item and proposed dispositions
  | UserConfirmed Text Text     -- ^ Item and chosen disposition
  | UserCorrected Text Text     -- ^ Item and user-provided location
  | FunctionChosen Text         -- ^ Space function selected
  | SessionConfirmedDone        -- ^ User confirmed session is done
  deriving (Show, Eq)
