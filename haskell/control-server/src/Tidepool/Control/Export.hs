{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Export training examples from LSP exploration.
module Tidepool.Control.Export
  ( exportTrainingExamples
  , discoverSymbols
  ) where

import Control.Monad (forM_, when)
import Control.Monad.Freer (runM)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (stdout, stderr, hFlush, hPutStrLn)

import Tidepool.Control.Scout.Teach.Gemma (extractCandidates)
import Tidepool.Control.Scout.LSP (locationToText)
import Tidepool.Effect.LSP
  ( workspaceSymbol, hover, textDocument, position
  , SymbolInformation(..), HoverInfo(..), Location(..), Range(..), Position(..)
  )
import Tidepool.LSP.Interpreter (LSPSession, runLSP)
import Tidepool.Training.Format (formatSelectSymbolsExample)

-- | Export training examples for given seed symbols.
exportTrainingExamples
  :: LSPSession
  -> [Text]      -- Seed symbol names
  -> IO ()
exportTrainingExamples session seeds = do
  forM_ seeds $ \seedName -> do
    -- 1. Find symbol via LSP
    symbols <- runM $ runLSP session $ workspaceSymbol seedName

    -- 2. For each result, fetch hover and extract candidates
    forM_ symbols $ \symInfo -> do
      let SymbolInformation symName _ loc _ = symInfo
          Location uri rng = loc
          file = case T.stripPrefix "file://" uri of
            Just f -> f
            Nothing -> uri
          Range startPosRec _ = rng
          Position line char = startPosRec
          startPos = position line char

      -- Get hover info
      maybeHover <- runM $ runLSP session $ hover (textDocument file) startPos

      let (sig, docs, code) = case maybeHover of
            Just hoverInfo ->
              let HoverInfo contents _ = hoverInfo
              in (contents, Nothing, Nothing)  -- TODO: Parse hover contents properly
            Nothing -> (symName, Nothing, Nothing)

      let candidates = extractCandidates sig

      hPutStrLn stderr $ "[Export] Symbol: " <> T.unpack symName
      hPutStrLn stderr $ "[Export] Signature (first 200 chars): " <> take 200 (T.unpack sig)
      hPutStrLn stderr $ "[Export] Candidates: " <> show candidates

      -- Skip if no candidates
      when (not $ null candidates) $ do
        let location = locationToText loc
        let jsonl = formatSelectSymbolsExample
                      symName
                      location
                      sig
                      code
                      docs
                      candidates

        -- Output JSONL line
        BL.putStrLn jsonl
        hFlush stdout

-- | Discover interesting symbols in the workspace.
discoverSymbols :: LSPSession -> IO [Text]
discoverSymbols session = do
  -- Query for common type/function patterns
  hPutStrLn stderr "[Export] Querying for 'Config'..."
  configSyms <- runM $ runLSP session $ workspaceSymbol "Config"
  hPutStrLn stderr $ "[Export] Found " <> show (length configSyms) <> " Config symbols"

  hPutStrLn stderr "[Export] Querying for empty string (all symbols)..."
  allSyms <- runM $ runLSP session $ workspaceSymbol ""
  hPutStrLn stderr $ "[Export] Found " <> show (length allSyms) <> " total symbols"

  -- If we got symbols, filter to interesting ones
  let filtered = if length allSyms > 100
        then take 20 allSyms  -- Just take first 20 if too many
        else allSyms

  -- Extract unique names
  let uniqueNames = nub $ map (\(SymbolInformation name _ _ _) -> name) filtered
  hPutStrLn stderr $ "[Export] Returning " <> show (length uniqueNames) <> " unique names"
  pure uniqueNames
