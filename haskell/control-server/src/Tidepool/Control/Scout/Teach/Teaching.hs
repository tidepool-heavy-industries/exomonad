{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Teaching interpreter for ScoutGemma effect.
--
-- This interpreter calls Haiku (via Anthropic API) instead of local FunctionGemma,
-- and records both the raw response and converted training data.
--
-- Resource ownership: Manages recording handles lifecycle using bracket pattern.
module Tidepool.Control.Scout.Teach.Teaching
  ( runScoutGemmaWithTeaching
  ) where

import Control.Monad.Freer (Eff, interpret, LastMember, sendM, runM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID.V4 as UUID

import Tidepool.Control.Scout.Teach.Gemma (ScoutGemma(..))
import Tidepool.Control.Scout.Teach.Teacher (ScoutGemmaEffect(..))
import Tidepool.Control.Scout.Teach.Types (LSPSymbol(..))
import Tidepool.Control.Scout.Tools
  ( SelectSymbolsTool(..)
  , SelectSymbolsInput(..)
  , SelectSymbolsOutput(..)
  )
import Tidepool.Teaching.Execute (executeWithTeaching)
import Tidepool.Teaching.Record (initRecording, closeRecording)
import Tidepool.Teaching.Types (AnthropicApiKey)

-- | Teaching interpreter: calls Haiku and records training data.
--
-- This interpreter bridges the ScoutGemma effect to Haiku API calls:
-- 1. Acquires recording handles (session initialization)
-- 2. Interprets SelectRelevantSymbols operations by:
--    - Flattening LSPSymbol into SelectSymbolsInput
--    - Calling executeWithTeaching (which calls Haiku and records)
--    - Extracting the result
-- 3. Cleans up recording handles (session finalization)
--
-- Resource lifecycle (bracket pattern):
-- @
--   acquire:  sessionId ← UUID, handles ← initRecording
--   use:      interpret effect operations (call Haiku)
--   cleanup:  closeRecording handles
-- @
runScoutGemmaWithTeaching
  :: forall effs a. LastMember IO effs
  => FilePath  -- ^ Output directory for training data
  -> AnthropicApiKey  -- ^ Anthropic API key
  -> Eff (ScoutGemma ': effs) a
  -> Eff effs a
runScoutGemmaWithTeaching outputDir apiKey action = do
  -- Resource acquisition
  sessionId <- sendM UUID.nextRandom
  handles <- sendM $ initRecording outputDir sessionId

  sendM $ putStrLn $ "[Teaching] Session " <> show sessionId <> " started"
  sendM $ putStrLn $ "[Teaching] Output directory: " <> outputDir

  -- Interpret ScoutGemma effect operations
  result <- interpret (\case
    SelectRelevantSymbols topic symbol candidates -> do
      sendM $ putStrLn $ "[Teaching] SelectRelevantSymbols for topic: " <> T.unpack topic
      sendM $ putStrLn $ "[Teaching] Calling Haiku (will record training data)..."

      -- Convert effect args to tool input (flatten LSPSymbol)
      let input = SelectSymbolsInput
            { topic = topic
            , symbolName = lsName symbol
            , symbolKind = T.pack $ show (lsKind symbol)
            , symbolLocation = T.pack $ show (lsLocation symbol)
            , symbolSignature = lsSignature symbol
            , symbolDocComment = lsDocComment symbol
            , candidates = candidates
            }

      -- Call teaching wrapper (Haiku API + recording)
      -- executeWithTeaching handles:
      -- - Calling Haiku with tool use
      -- - Recording raw Anthropic response
      -- - Converting to FunctionGemma format
      -- - Recording converted training data
      output <- sendM $ runM $
        executeWithTeaching @SelectSymbolsTool @ScoutGemmaEffect
          apiKey handles SelectSymbolsTool input

      sendM $ putStrLn $ "[Teaching] Haiku selected " <> show (length (selected output)) <> " symbols"

      -- Extract result
      pure (selected output)
    ) action

  -- Resource cleanup
  sendM $ putStrLn $ "[Teaching] Session " <> show sessionId <> " complete, closing handles"
  sendM $ closeRecording handles

  pure result
