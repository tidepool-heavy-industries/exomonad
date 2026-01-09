{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Merge accumulator for collecting parallel worker results.
--
-- This module provides the mechanism for gathering results from parallel
-- workers at a Merge node. Results are grouped by correlation key and
-- accumulated until all expected sources have reported.
module Tidepool.Parallel.Merge
  ( -- * Merge Accumulator
    MergeAccumulator(..)
  , newMergeAccumulator
  , addResult
  , checkComplete
  , getCompletedResults

    -- * Partial Results
  , PartialMerge(..)
  , MergeSlot(..)

    -- * Type-level extraction
  , ExpectedSources(..)
  , ExtractMergeResults(..)

    -- * Type families
  , FromPayloads
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically)
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
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))

import Tidepool.Graph.Types (HList(..), From)


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


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE FAMILIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract payload types from a list of 'From' markers.
--
-- @
-- FromPayloads '[From "a" Int, From "b" String] = '[Int, String]
-- @
type FromPayloads :: [Type] -> [Type]
type family FromPayloads sources where
  FromPayloads '[] = '[]
  FromPayloads (From name payload ': rest) = payload ': FromPayloads rest


-- ════════════════════════════════════════════════════════════════════════════
-- EXTRACT MERGE RESULTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for extracting typed HList from a HashMap of JSON values.
--
-- This enables type-safe extraction of merge results. Each source's JSON
-- value is parsed to its expected type and assembled into an HList.
--
-- @
-- results <- extractMergeResults @'[From "a" Int, From "b" String] slots
-- -- results :: Either Text (HList '[Int, String])
-- @
type ExtractMergeResults :: [Type] -> Constraint
class ExtractMergeResults sources where
  extractMergeResults :: HashMap Text Value -> Either Text (HList (FromPayloads sources))

instance ExtractMergeResults '[] where
  extractMergeResults _ = Right HNil

instance
  ( KnownSymbol name
  , FromJSON payload
  , ExtractMergeResults rest
  ) => ExtractMergeResults (From name payload ': rest) where
  extractMergeResults slots = do
    let sourceName = textVal @name
    case HM.lookup sourceName slots of
      Nothing -> Left $ "Missing source: " <> sourceName
      Just val -> case parseEither parseJSON val of
        Left err -> Left $ "Failed to parse " <> sourceName <> ": " <> T.pack err
        Right payload -> do
          rest <- extractMergeResults @rest slots
          Right (payload ::: rest)


-- ════════════════════════════════════════════════════════════════════════════
-- GET COMPLETED RESULTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get the completed results for a correlation key as a typed HList.
--
-- Returns 'Nothing' if the key doesn't exist or isn't complete.
-- Returns 'Left err' if parsing fails for any result.
-- Returns 'Right hlist' with the typed results in source order.
--
-- @
-- acc <- newMergeAccumulator @'[From "a" Int, From "b" String]
-- -- ... add results ...
-- result <- getCompletedResults @'[From "a" Int, From "b" String] acc key
-- case result of
--   Nothing -> putStrLn "Not complete yet"
--   Just (Left err) -> putStrLn $ "Parse error: " <> err
--   Just (Right (a ::: b ::: HNil)) -> print (a, b)
-- @
getCompletedResults
  :: forall sources key.
     ( ExtractMergeResults sources
     , Hashable key
     )
  => MergeAccumulator key
  -> key
  -> IO (Maybe (Either Text (HList (FromPayloads sources))))
getCompletedResults acc key = do
  partials <- readTVarIO (acc.maPartials)
  case HM.lookup key partials of
    Nothing -> pure Nothing
    Just partial -> do
      let slots = extractSlots (partial.pmSlots)
      pure $ Just $ extractMergeResults @sources slots
  where
    extractSlots :: HashMap Text MergeSlot -> HashMap Text Value
    extractSlots = HM.mapMaybe $ \case
      SlotEmpty -> Nothing
      SlotFilled v -> Just v
