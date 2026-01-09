{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Budget tracking for ClaudeCode execution.
--
-- Tracks spending across ClaudeCode calls and enforces limits.
-- On budget exceed: warns, blocks for user approval, then resets.
--
-- = Usage
--
-- @
-- let cfg = BudgetConfig { bcLimit = Just (usdCost 10.0), bcWarnAt = Just 0.8 }
-- (result, finalBudget) <- runBudgetIO cfg $ do
--   ... ClaudeCode calls ...
-- putStrLn $ "Total spent: $" <> show (costUsd $ bsSpent finalBudget)
-- @
module Tidepool.ClaudeCode.Budget
  ( -- * Cost Types
    Cost(..)
  , TokenUsage(..)
  , zeroCost
  , usdCost
  , fromClaudeCodeResult

    -- * Budget Effect
  , Budget(..)
  , recordSpend
  , checkBudget
  , getSpent
  , getRemaining

    -- * Configuration
  , BudgetConfig(..)
  , defaultBudgetConfig
  , unlimitedBudget

    -- * State
  , BudgetState(..)
  , SpendEvent(..)

    -- * Interpreter
  , runBudgetIO

    -- * Reporting
  , reportBudget
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
import Control.Monad.Freer (Eff, Member, LastMember, send, interpret, sendM)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import Tidepool.ClaudeCode.Types (ClaudeCodeResult(..), ModelUsage(..))


-- ============================================================================
-- Cost Types
-- ============================================================================

-- | Cost of a ClaudeCode execution.
data Cost = Cost
  { costUsd :: !Double
    -- ^ Cost in USD
  , costTokens :: !TokenUsage
    -- ^ Token breakdown
  , costSource :: !Text
    -- ^ Source identifier (e.g., "claude-code", model name)
  }
  deriving stock (Show, Eq)

instance Semigroup Cost where
  a <> b = Cost
    { costUsd = costUsd a + costUsd b
    , costTokens = costTokens a <> costTokens b
    , costSource = combineSources (costSource a) (costSource b)
    }
    where
      combineSources s1 s2
        | T.null s1 = s2
        | T.null s2 = s1
        | otherwise = s1 <> " + " <> s2

instance Monoid Cost where
  mempty = zeroCost

-- | Token usage breakdown.
data TokenUsage = TokenUsage
  { tuInput :: !Int
  , tuOutput :: !Int
  , tuCacheRead :: !Int
  , tuCacheCreate :: !Int
  }
  deriving stock (Show, Eq)

instance Semigroup TokenUsage where
  a <> b = TokenUsage
    { tuInput = tuInput a + tuInput b
    , tuOutput = tuOutput a + tuOutput b
    , tuCacheRead = tuCacheRead a + tuCacheRead b
    , tuCacheCreate = tuCacheCreate a + tuCacheCreate b
    }

instance Monoid TokenUsage where
  mempty = TokenUsage 0 0 0 0

-- | Zero cost.
zeroCost :: Cost
zeroCost = Cost 0 mempty ""

-- | Create a cost from just USD amount.
usdCost :: Double -> Cost
usdCost usd = Cost usd mempty "manual"

-- | Extract cost from ClaudeCodeResult.
fromClaudeCodeResult :: ClaudeCodeResult -> Cost
fromClaudeCodeResult ccr = Cost
  { costUsd = ccrTotalCostUsd ccr
  , costTokens = aggregateTokens (ccrModelUsage ccr)
  , costSource = "claude-code"
  }
  where
    aggregateTokens :: Map.Map Text ModelUsage -> TokenUsage
    aggregateTokens = foldMap $ \mu -> TokenUsage
      { tuInput = muInputTokens mu
      , tuOutput = muOutputTokens mu
      , tuCacheRead = muCacheReadInputTokens mu
      , tuCacheCreate = muCacheCreationInputTokens mu
      }


-- ============================================================================
-- Budget Effect
-- ============================================================================

-- | Budget effect for tracking ClaudeCode spending.
data Budget r where
  -- | Record spending from a ClaudeCode call.
  RecordSpend :: Cost -> Budget ()

  -- | Check if within budget. Blocks for approval if exceeded.
  CheckBudget :: Budget ()

  -- | Get total spent so far.
  GetSpent :: Budget Cost

  -- | Get remaining budget (Nothing = unlimited).
  GetRemaining :: Budget (Maybe Cost)

-- | Record spending from a ClaudeCode call.
recordSpend :: Member Budget effs => Cost -> Eff effs ()
recordSpend = send . RecordSpend

-- | Check if within budget. Blocks for approval if exceeded.
checkBudget :: Member Budget effs => Eff effs ()
checkBudget = send CheckBudget

-- | Get total spent so far.
getSpent :: Member Budget effs => Eff effs Cost
getSpent = send GetSpent

-- | Get remaining budget (Nothing = unlimited).
getRemaining :: Member Budget effs => Eff effs (Maybe Cost)
getRemaining = send GetRemaining


-- ============================================================================
-- Configuration
-- ============================================================================

-- | Budget configuration.
data BudgetConfig = BudgetConfig
  { bcLimit :: !(Maybe Cost)
    -- ^ Maximum allowed spending (Nothing = unlimited)
  , bcWarnAt :: !(Maybe Double)
    -- ^ Warn when this percentage of budget is used (e.g., 0.8 = 80%)
  }
  deriving stock (Show, Eq)

-- | Default budget config: $10 limit, warn at 80%.
defaultBudgetConfig :: BudgetConfig
defaultBudgetConfig = BudgetConfig
  { bcLimit = Just (usdCost 10.0)
  , bcWarnAt = Just 0.8
  }

-- | Unlimited budget (no limits, no warnings).
unlimitedBudget :: BudgetConfig
unlimitedBudget = BudgetConfig Nothing Nothing


-- ============================================================================
-- State
-- ============================================================================

-- | Budget state during execution.
data BudgetState = BudgetState
  { bsSpent :: !Cost
    -- ^ Total spent so far
  , bsConfig :: !BudgetConfig
    -- ^ Configuration
  , bsHistory :: ![SpendEvent]
    -- ^ Spending history (most recent first, max 100 entries)
  , bsApprovals :: !Int
    -- ^ Number of budget approvals granted
  , bsWarned :: !Bool
    -- ^ Whether warning threshold has been shown this cycle
  }
  deriving stock (Show)

-- | A single spending event.
data SpendEvent = SpendEvent
  { seTime :: !UTCTime
  , seCost :: !Cost
  }
  deriving stock (Show)


-- ============================================================================
-- Interpreter
-- ============================================================================

-- | Run Budget effect with IO for user approval prompts.
--
-- Returns the result along with final budget state for reporting.
runBudgetIO
  :: LastMember IO effs
  => BudgetConfig
  -> Eff (Budget ': effs) a
  -> Eff effs (a, BudgetState)
runBudgetIO cfg eff = do
  stateVar <- sendM $ newTVarIO (BudgetState zeroCost cfg [] 0 False)
  result <- interpret (handleBudget stateVar) eff
  finalState <- sendM $ readTVarIO stateVar
  pure (result, finalState)

handleBudget :: LastMember IO effs => TVar BudgetState -> Budget r -> Eff effs r
handleBudget stateVar = \case
  RecordSpend cost -> sendM $ do
    now <- getCurrentTime
    atomically $ modifyTVar' stateVar $ \bs ->
      bs { bsSpent = bsSpent bs <> cost
         , bsHistory = take 100 (SpendEvent now cost : bsHistory bs)  -- Cap history
         }
    -- Log the spend
    total <- costUsd . bsSpent <$> readTVarIO stateVar
    printf "[BUDGET] Spent: $%.4f (total: $%.4f)\n" (costUsd cost) total

  CheckBudget -> sendM $ do
    bs <- readTVarIO stateVar
    let spent = costUsd (bsSpent bs)
        limitUsd = maybe 0 costUsd (bcLimit (bsConfig bs))

    -- Check warning threshold (only warn once per budget cycle)
    case (bcWarnAt (bsConfig bs), bcLimit (bsConfig bs)) of
      (Just warnPct, Just _)
        | not (bsWarned bs)
        , limitUsd > 0
        , spent >= warnPct * limitUsd -> do
            atomically $ modifyTVar' stateVar $ \bs' -> bs' { bsWarned = True }
            printf "[BUDGET] Warning: %.0f%% of budget used ($%.2f / $%.2f)\n"
              (spent / limitUsd * 100)
              spent
              limitUsd
      _ -> pure ()

    -- Check limit
    case bcLimit (bsConfig bs) of
      Nothing -> pure ()  -- Unlimited
      Just limit
        | spent > costUsd limit -> do
            printf "\n[BUDGET] ⚠️  EXCEEDED: $%.2f spent (limit: $%.2f)\n"
              spent (costUsd limit)
            putStr "Continue with another budget cycle? [y/N] "
            hFlush stdout
            response <- getLine
            if response `elem` ["y", "Y", "yes", "Yes"]
              then do
                putStrLn "[BUDGET] Approved. Resetting budget."
                atomically $ modifyTVar' stateVar $ \bs' ->
                  bs' { bsSpent = zeroCost
                      , bsApprovals = bsApprovals bs' + 1
                      , bsWarned = False  -- Reset warning flag for new cycle
                      }
              else
                fail $ "Budget exceeded: $" <> show spent <> " (limit: $" <> show (costUsd limit) <> ")"
        | otherwise -> pure ()

  GetSpent -> sendM $ bsSpent <$> readTVarIO stateVar

  GetRemaining -> sendM $ do
    bs <- readTVarIO stateVar
    pure $ case bcLimit (bsConfig bs) of
      Nothing -> Nothing
      Just limit -> Just $ Cost
        { costUsd = max 0 (costUsd limit - costUsd (bsSpent bs))
        , costTokens = mempty
        , costSource = ""
        }


-- ============================================================================
-- Reporting
-- ============================================================================

-- | Generate a budget report.
reportBudget :: BudgetState -> Text
reportBudget bs = T.unlines
  [ "## Cost Report"
  , ""
  , "**Total spent**: $" <> T.pack (printf "%.4f" (costUsd $ bsSpent bs))
  , "**Budget limit**: " <> maybe "unlimited" (\c -> "$" <> T.pack (printf "%.2f" (costUsd c))) (bcLimit $ bsConfig bs)
  , "**Approvals granted**: " <> T.pack (show (bsApprovals bs))
  , ""
  , "### Token Usage"
  , "- Input: " <> T.pack (show (tuInput $ costTokens $ bsSpent bs))
  , "- Output: " <> T.pack (show (tuOutput $ costTokens $ bsSpent bs))
  , "- Cache read: " <> T.pack (show (tuCacheRead $ costTokens $ bsSpent bs))
  , "- Cache create: " <> T.pack (show (tuCacheCreate $ costTokens $ bsSpent bs))
  , ""
  , "### History (" <> T.pack (show (length $ bsHistory bs)) <> " calls)"
  , T.unlines $ map formatEvent (take 10 $ bsHistory bs)
  ]
  where
    formatEvent SpendEvent{..} =
      "- $" <> T.pack (printf "%.4f" (costUsd seCost)) <>
      " (" <> costSource seCost <> ")"
