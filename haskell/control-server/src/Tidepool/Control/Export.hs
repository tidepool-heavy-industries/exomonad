{-# LANGUAGE OverloadedStrings #-}

-- | Export training examples from LSP exploration.
--
-- Generates FunctionGemma training data in 3-turn token format with holes
-- for human annotation. Uses LSP (workspace/symbol, hover) to extract
-- type signatures and candidates.
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
  , SymbolKind(..)
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

      -- Skip if no candidates (e.g., simple types with no references)
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
--
-- Filters to functions only, since data types have minimal hover info
-- (just ":: Type") while functions have full signatures with type references.
discoverSymbols :: LSPSession -> IO [Text]
discoverSymbols session = do
  allSyms <- runM $ runLSP session $ workspaceSymbol ""

  -- Filter to functions only (SKFunction, SKMethod, SKVariable)
  -- Data types (SKClass, SKStruct, SKEnum) just show ":: Type"
  let isFunctionLike (SymbolInformation _ kind _ _) = kind `elem` [SKFunction, SKMethod, SKVariable]
      functions = filter isFunctionLike allSyms

  -- Filter out derived instances ($fShow*, $fEq*, etc.)
  let isNotDerived (SymbolInformation name _ _ _) = not ("$f" `T.isPrefixOf` name)
      userDefined = filter isNotDerived functions

  -- Take reasonable sample
  let filtered = take 50 userDefined

  -- Extract unique names
  let uniqueNames = nub $ map (\(SymbolInformation name _ _ _) -> name) filtered

  hPutStrLn stderr $ "Discovered " <> show (length uniqueNames) <> " functions from "
    <> show (length allSyms) <> " workspace symbols"
  pure uniqueNames
