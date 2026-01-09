{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Merge accumulator for collecting parallel worker results.
--
-- This module provides the mechanism for gathering results from parallel
-- workers at a Merge node. Results are grouped by correlation key and
-- accumulated until all expected sources have reported.
module Tidepool.Parallel.Merge
  ( -- * Merge Accumulator
    MergeAccumulator
  , newMergeAccumulator
  , addResult
  , checkComplete

    -- * Partial Results
  , PartialMerge(..)
  , MergeSlot(..)

    -- * Type-level extraction
  , ExpectedSources(..)
  ) where

import Control.Concurrent.STM (STM, TVar, newTVarIO, readTVar, writeTVar, atomically)
import Data.Aeson (Value, FromJSON(..))
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Kind (Type, Constraint)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))

import Tidepool.Graph.Types (HList(..), CorrelateBy(..), From)


-- ════════════════════════════════════════════════════════════════════════════
-- MERGE ACCUMULATOR
-- ════════════════════════════════════════════════════════════════════════════

-- | Accumulator for collecting results from parallel workers.
--
-- Tracks partial results grouped by correlation key. When all expected
-- sources have reported for a key, the complete HList can be extracted.
data MergeAccumulator key = MergeAccumulator
  { maExpectedSources :: HashSet Text
    -- ^ Source names we expect results from
  , maPartials :: TVar (HashMap key PartialMerge)
    -- ^ Partial results by correlation key
  }

-- | Create a new merge accumulator.
--
-- @
-- acc <- newMergeAccumulator @'[From "payment" PayResult, From "inventory" InvResult]
-- @
newMergeAccumulator
  :: forall sources key.
     ( ExpectedSources sources
     , Hashable key
     )
  => IO (MergeAccumulator key)
newMergeAccumulator = do
  partialsVar <- newTVarIO HM.empty
  pure MergeAccumulator
    { maExpectedSources = expectedSources @sources
    , maPartials = partialsVar
    }


-- ════════════════════════════════════════════════════════════════════════════
-- PARTIAL RESULTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Partially accumulated merge results for a single correlation key.
data PartialMerge = PartialMerge
  { pmSlots :: HashMap Text MergeSlot
    -- ^ Results by source name
  }
  deriving stock (Show)

-- | A single slot in a partial merge.
data MergeSlot
  = SlotEmpty
  | SlotFilled Value
  deriving stock (Show, Eq)

-- | Add a result to the accumulator.
--
-- Returns 'Just' the completed source set if this result completed a key,
-- 'Nothing' otherwise.
addResult
  :: (Hashable key, Eq key)
  => MergeAccumulator key
  -> key           -- ^ Correlation key
  -> Text          -- ^ Source name
  -> Value         -- ^ Result payload
  -> IO (Maybe (HashSet Text))  -- ^ Completed sources if now complete
addResult acc key source payload = atomically $ do
  partials <- readTVar (acc.maPartials)
  let partial = HM.lookupDefault emptyPartial key partials
      partial' = partial { pmSlots = HM.insert source (SlotFilled payload) (partial.pmSlots) }
      partials' = HM.insert key partial' partials
  writeTVar (acc.maPartials) partials'

  -- Check if complete
  let filled = HM.keysSet $ HM.filter isFilled (partial'.pmSlots)
  if filled == acc.maExpectedSources
    then pure (Just filled)
    else pure Nothing
  where
    emptyPartial = PartialMerge HM.empty
    isFilled SlotEmpty = False
    isFilled (SlotFilled _) = True

-- | Check if a correlation key has all expected results.
checkComplete
  :: (Hashable key, Eq key)
  => MergeAccumulator key
  -> key
  -> IO Bool
checkComplete acc key = do
  partials <- readTVarIO (acc.maPartials)
  case HM.lookup key partials of
    Nothing -> pure False
    Just partial -> do
      let filled = HM.keysSet $ HM.filter isFilled (partial.pmSlots)
      pure $ filled == acc.maExpectedSources
  where
    isFilled SlotEmpty = False
    isFilled (SlotFilled _) = True

-- | Read a TVar in IO.
readTVarIO :: TVar a -> IO a
readTVarIO = atomically . readTVar


-- ════════════════════════════════════════════════════════════════════════════
-- EXPECTED SOURCES TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for extracting expected source names from a Merge type.
type ExpectedSources :: [Type] -> Constraint
class ExpectedSources sources where
  expectedSources :: HashSet Text

instance ExpectedSources '[] where
  expectedSources = HS.empty

instance (KnownSymbol name, ExpectedSources rest) => ExpectedSources (From name payload ': rest) where
  expectedSources = HS.insert (textVal @name) (expectedSources @rest)

-- | Get text value of a type-level symbol.
textVal :: forall name. KnownSymbol name => Text
textVal = T.pack (symbolVal (Proxy @name))
