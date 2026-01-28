-- | Cabal interpreter - interprets the Cabal effect to IO.
--
-- Runs cabal commands in subprocesses and parses test output into
-- structured failure information for LLM consumption.
--
-- = Usage
--
-- @
-- import ExoMonad.Cabal.Interpreter
--
-- -- Only output parsing functions are provided here now.
-- -- Execution is handled by ExoMonad.Control.Effects.Cabal via docker-ctl.
-- @
module ExoMonad.Cabal.Interpreter
  ( -- * Test Output Parsing
    parseQuickCheckOutput
  , parseHSpecOutput
  , parseTestOutput

    -- * Re-exports
  , Cabal(..)
  , CabalResult(..)
  , TestFailure(..)
  , cabalBuild
  , cabalTest
  , cabalClean
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

import ExoMonad.Effects.Cabal


-- ════════════════════════════════════════════════════════════════════════════
-- TEST OUTPUT PARSING
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse test output from any supported framework.
--
-- Tries QuickCheck first, then HSpec. Returns all failures found.
parseTestOutput :: Text -> [TestFailure]
parseTestOutput output =
  parseQuickCheckOutput output ++ parseHSpecOutput output

-- | Parse QuickCheck test output.
--
-- Looks for patterns like:
--
-- @
-- *** Failed! Falsified (after 5 tests):
-- [1,2,3]
-- prop_pushPop:                             FAIL
-- @
parseQuickCheckOutput :: Text -> [TestFailure]
parseQuickCheckOutput output = mapMaybe (parseQCFailure output) chunks
  where
    -- Split on "*** Failed!" to find failure blocks
    chunks = drop 1 $ T.splitOn "*** Failed!" output

-- | Parse a single QuickCheck failure block.
--
-- Takes the full output for context extraction.
parseQCFailure :: Text -> Text -> Maybe TestFailure
parseQCFailure fullOutput chunk = do
  -- Extract the failure message (e.g., "Falsified (after 5 tests):")
  let lines' = T.lines chunk
  case lines' of
    [] -> Nothing
    (firstLine:rest) -> do
      let message = "Failed!" <> T.strip firstLine

      -- Extract counterexample (usually the next few lines before another test)
      let counterexample = extractCounterexample rest

      -- Try to extract the property name from context
      let propName = extractPropertyName chunk fullOutput

      -- Try to extract seed
      let seed = extractSeed chunk

      Just TestFailure
        { tfPropertyName = propName
        , tfMessage = message
        , tfCounterexample = counterexample
        , tfSeed = seed
        , tfLocation = Nothing
        }

-- | Extract counterexample from lines following "Failed!".
extractCounterexample :: [Text] -> Maybe Text
extractCounterexample lines' =
  let relevant = takeWhile isCounterexampleLine lines'
  in if null relevant
     then Nothing
     else Just $ T.strip $ T.unlines relevant
  where
    isCounterexampleLine line =
      not (T.null (T.strip line)) &&
      not ("prop_" `T.isPrefixOf` T.strip line) &&
      not ("***" `T.isPrefixOf` T.strip line) &&
      not ("====" `T.isPrefixOf` T.strip line) &&
      not ("FAIL" `T.isSuffixOf` T.strip line) &&
      not ("PASS" `T.isSuffixOf` T.strip line)

-- | Extract property name from failure context.
--
-- Looks for patterns like "prop_name: FAIL" or "Testing prop_name..."
extractPropertyName :: Text -> Text -> Text
extractPropertyName chunk fullOutput =
  -- First look backwards in full output for context (more reliable)
  case findPropBeforeChunk chunk fullOutput of
    Just name -> name
    Nothing ->
      -- Fall back to finding prop_name in the chunk itself
      case findPropInChunk chunk of
        Just name -> name
        Nothing -> "unknown_property"
  where
    findPropInChunk c =
      let matches = (T.unpack c :: String) =~ ("prop_[a-zA-Z0-9_]+" :: String) :: [[String]]
      in case matches of
           ((m:_):_) -> Just (T.pack m)
           _ -> Nothing

    findPropBeforeChunk _c full =
      -- Find where chunk appears in full output, look before it
      let (before, _) = T.breakOn "*** Failed!" full
          -- Get last prop_ mention before the failure
          matches = (T.unpack before :: String) =~ ("prop_[a-zA-Z0-9_]+" :: String) :: [[String]]
      in case reverse matches of
           ((m:_):_) -> Just (T.pack m)
           _ -> Nothing

-- | Extract QuickCheck seed from output.
--
-- Looks for patterns like "Use --seed 12345 to reproduce"
extractSeed :: Text -> Maybe Int
extractSeed chunk =
  let matches = (T.unpack chunk :: String) =~ ("--seed[= ]([0-9]+)" :: String) :: [[String]]
  in case matches of
       ((_:seedStr:_):_) -> case reads seedStr of
         [(n, "")] -> Just n
         _ -> Nothing
       _ -> Nothing


-- | Parse HSpec test output.
--
-- Looks for patterns like:
--
-- @
-- 1) MyModule.functionName should do something
--    expected: True
--     but got: False
-- @
parseHSpecOutput :: Text -> [TestFailure]
parseHSpecOutput output = mapMaybe parseHSpecFailure chunks
  where
    -- HSpec numbers failures: "1)", "2)", etc.
    chunks = drop 1 $ T.splitOn "\n  " filtered
    -- Filter to lines that look like HSpec failures
    filtered = output

-- | Parse a single HSpec failure.
parseHSpecFailure :: Text -> Maybe TestFailure
parseHSpecFailure chunk = do
  let lines' = T.lines chunk
  case lines' of
    [] -> Nothing
    (firstLine:rest) ->
      -- Check if this looks like a numbered failure
      if isHSpecFailureHeader firstLine
        then do
          let name = extractHSpecTestName firstLine
              message = T.unlines $ take 5 rest  -- Include context
              location = extractHSpecLocation (T.unlines rest)
          Just TestFailure
            { tfPropertyName = name
            , tfMessage = T.strip message
            , tfCounterexample = Nothing
            , tfSeed = Nothing
            , tfLocation = location
            }
        else Nothing

-- | Check if line is an HSpec failure header (starts with number and paren).
isHSpecFailureHeader :: Text -> Bool
isHSpecFailureHeader line =
  let stripped = T.strip line
  in case T.uncons stripped of
       Just (c, rest) -> c >= '1' && c <= '9' && ")" `T.isPrefixOf` T.dropWhile (/= ')') rest
       Nothing -> False

-- | Extract test name from HSpec failure header.
extractHSpecTestName :: Text -> Text
extractHSpecTestName line =
  -- "1) MyModule.functionName should do something" -> "functionName should do something"
  let afterNum = T.dropWhile (/= ')') line
      afterParen = T.drop 1 afterNum
  in T.strip afterParen

-- | Extract source location from HSpec output.
extractHSpecLocation :: Text -> Maybe Text
extractHSpecLocation output =
  let matches = (T.unpack output :: String) =~ ("([a-zA-Z0-9_/]+\\.hs):([0-9]+)" :: String) :: [[String]]
  in case matches of
       ((m:_):_) -> Just (T.pack m)
       _ -> Nothing
