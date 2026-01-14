-- | Native graph runner for Urchin Prime.
--
-- Executes the UrchinPrimeGraph using Git and BD effects.
-- This is a native runner (not WASM) for CLI use.
--
-- = Usage
--
-- @
-- import Tidepool.BD.Prime.Runner (runUrchinPrime)
--
-- main = do
--   result <- runUrchinPrime
--   TIO.putStrLn result
-- @
module Tidepool.BD.Prime.Runner
  ( -- * Runner
    runUrchinPrime
  , runUrchinPrimeWithConfig
  , runUrchinPrimeJSON
  , runUrchinPrimeJSONWithConfig

    -- * Re-exports
  , BDConfig(..)
  , defaultBDConfig
  ) where

import Control.Monad.Freer (Eff, runM)
import Data.Text (Text)

import Tidepool.Graph.Goto.Internal (OneOf(..), GotoChoice(..))

import Tidepool.Effects.Git (WorktreeInfo)
import Tidepool.BD.Interpreter (BDConfig(..), defaultBDConfig, runBDIO)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.BD.Prime.Graph (UrchinPrimeGraph(..), PrimeContext, primeHandlers, renderPrime, renderPrimeJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run urchin prime and return the rendered markdown.
--
-- Uses default BD configuration (auto-discovery).
runUrchinPrime :: IO Text
runUrchinPrime = runUrchinPrimeWithConfig defaultBDConfig


-- | Run urchin prime with custom BD configuration, returning markdown.
runUrchinPrimeWithConfig :: BDConfig -> IO Text
runUrchinPrimeWithConfig config = do
  result <- runUrchinPrimeRaw config
  pure $ case result of
    Left errMsg -> errMsg
    Right ctx   -> renderPrime ctx


-- | Run urchin prime and return JSON output.
--
-- Uses default BD configuration (auto-discovery).
runUrchinPrimeJSON :: IO Text
runUrchinPrimeJSON = runUrchinPrimeJSONWithConfig defaultBDConfig


-- | Run urchin prime with custom BD configuration, returning JSON.
runUrchinPrimeJSONWithConfig :: BDConfig -> IO Text
runUrchinPrimeJSONWithConfig config = do
  result <- runUrchinPrimeRaw config
  pure $ case result of
    Left errMsg -> errMsg
    Right ctx   -> renderPrimeJSON ctx


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the graph up to gather phase, returning either error message or context.
--
-- This executes Detect → Gather but skips Render, allowing the caller
-- to choose the output format.
runUrchinPrimeRaw :: BDConfig -> IO (Either Text PrimeContext)
runUrchinPrimeRaw config = runM $ runGitIO $ runBDIO config $ do
  -- Start at entry with ()
  detectResult <- primeHandlers.upDetect ()

  case detectResult of
    -- Goto "upGather" WorktreeInfo
    GotoChoice (Here worktreeInfo) -> do
      gatherResult <- primeHandlers.upGather worktreeInfo

      case gatherResult of
        -- Goto "upRender" PrimeContext - we have context, skip render node
        GotoChoice (Here primeCtx) -> pure (Right primeCtx)

        -- Goto Exit Text (early exit from gather)
        GotoChoice (There (Here errorMsg)) -> pure (Left errorMsg)
        GotoChoice (There (There impossible)) -> absurdOneOf impossible

    -- Goto Exit Text (early exit from detect - not in git repo)
    GotoChoice (There (Here errorMsg)) -> pure (Left errorMsg)
    GotoChoice (There (There impossible)) -> absurdOneOf impossible


-- | Handle impossible cases (empty OneOf).
--
-- This should never be reached due to type-level constraints.
absurdOneOf :: OneOf '[] -> a
absurdOneOf x = case x of {}
