{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FieldSelectors #-}
-- | DM Output Types
--
-- = Why This Module Exists
--
-- Types are separated from schema derivation ('DM.Output') because:
--
-- 1. @getDoc@ (TH) can only retrieve Haddock comments from /compiled/ modules
-- 2. @FieldSelectors@ must be enabled (project uses @NoFieldSelectors@)
-- 3. Field docs must use @-- ^@ format (after field, not before)
--
-- = Pattern for Schema-Derived Types
--
-- @
-- -- In Types module (with FieldSelectors):
-- data Foo = Foo
--   { bar :: Int
--     -- ^ Description for bar (required!)
--   }
--
-- -- In main module:
-- import Types (Foo(..))
-- fooSchema = $(deriveJSONSchema ''Foo)  -- pulls docs automatically
-- @
--
-- If a field lacks docs, compilation fails with a helpful error.
module DM.Output.Types
  ( -- * Turn Output
    TurnOutput(..)
  , NarrativeConnector(..)
  , emptyTurnOutput

    -- * Compression Output
  , CompressionOutput(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TURN OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Narrative connector - forces causal thinking
-- "And then" is lazy; these connectors demand causality
data NarrativeConnector
  = Therefore   -- This happened because of what came before
  | But         -- This happened despite what came before
  | Meanwhile   -- Parallel action (use sparingly)
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Simplified turn output that fits within API grammar limits.
-- NPC dialogue should be included directly in narration.
data TurnOutput = TurnOutput
  { narration :: Text
    -- ^ Narrative prose describing what happens this turn. Include NPC dialogue inline.
  , narrativeConnector :: Maybe NarrativeConnector
    -- ^ How this beat connects causally to previous events.
  , stressDelta :: Int
    -- ^ Change in stress from -9 to +9. Positive for costs, negative for relief.
  , coinDelta :: Int
    -- ^ Change in coin. Positive for gains, negative for expenses.
  , heatDelta :: Int
    -- ^ Change in heat from 0 to +4. Heat draws faction attention.
  , continueScene :: Bool
    -- ^ True to continue the current scene, False to end it.
  , costDescription :: Maybe Text
    -- ^ If there was a cost or setback, describe it for consequence echoing.
  , threatDescription :: Maybe Text
    -- ^ If there is an unresolved threat, describe it for future echoing.
  , suggestedActions :: [Text]
    -- ^ 2-3 suggested next actions the player might take.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

emptyTurnOutput :: TurnOutput
emptyTurnOutput = TurnOutput
  { narration = ""
  , narrativeConnector = Nothing
  , stressDelta = 0
  , coinDelta = 0
  , heatDelta = 0
  , continueScene = True
  , costDescription = Nothing
  , threatDescription = Nothing
  , suggestedActions = []
  }

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION OUTPUT
-- ══════════════════════════════════════════════════════════════

-- | Simplified compression output
data CompressionOutput = CompressionOutput
  { summary :: Text
    -- ^ One paragraph summary of what happened during the compressed segment.
  , keyMoments :: Text
    -- ^ Comma-separated list of key dramatic moments worth remembering.
  , consequenceSeeds :: Text
    -- ^ Comma-separated seeds for future consequences to echo.
  , stressChange :: Int
    -- ^ Net stress change over the compressed segment.
  , coinChange :: Int
    -- ^ Net coin change over the compressed segment.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
