{-# LANGUAGE OverloadedStrings #-}

module Tidepool.Control.StopHook.ErrorParser
  ( parseGHCOutput
  , classifyError
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

import Tidepool.Control.StopHook.Types

-- | Parse GHC error output into structured errors
parseGHCOutput :: Text -> [GHCError]
parseGHCOutput output =
  mapMaybe parseErrorLine $ T.lines output

-- | Parse a single error line
-- Format: path/to/File.hs:line:col: error: message
-- or: path/to/File.hs:line:col: warning: message
parseErrorLine :: Text -> Maybe GHCError
parseErrorLine line =
  let pattern = "^([^:]+):([0-9]+):([0-9]+): (error|warning): (.*)$" :: Text
      matches = (line =~ pattern) :: (Text, Text, Text, [Text])
  in case matches of
    (_, _, _, [file, lineNum, col, severity, msg]) -> Just $ GHCError
      { geFile = T.unpack file
      , geLine = read (T.unpack lineNum)
      , geColumn = read (T.unpack col)
      , geMessage = msg
      , geErrorType = classifyError msg
      , geSeverity = if severity == "error" then ErrorSeverity else WarningSeverity
      }
    _ -> Nothing

-- | Classify error by pattern matching on message
classifyError :: Text -> GHCErrorType
classifyError msg
  | "Couldn't match type" `T.isInfixOf` msg = TypeError $ parseTypeMismatch msg
  | "Not in scope" `T.isInfixOf` msg = ScopeError $ parseScopeError msg
  | "parse error" `T.isInfixOf` msg = ParseError msg
  | "Ambiguous type variable" `T.isInfixOf` msg = TypeError $ AmbiguousType msg
  | "No instance for" `T.isInfixOf` msg = TypeError $ MissingInstance msg
  | otherwise = OtherError msg

parseTypeMismatch :: Text -> TypeErrorInfo
parseTypeMismatch msg =
  -- Extract expected and actual types from "Couldn't match type 'X' with 'Y'"
  let pattern = "Couldn't match type \8216([^\8217]+)\8217 with \8216([^\8217]+)\8217" :: Text -- Handle smart quotes
      matches = (msg =~ pattern) :: (Text, Text, Text, [Text])
  in case matches of
    (_, _, _, [expected, actual]) -> TypeMismatch expected actual
    _ -> 
      -- Try regular quotes if smart quotes didn't match
      let pattern' = "Couldn't match type '([^']+)' with '([^']+)'" :: Text
          matches' = (msg =~ pattern') :: (Text, Text, Text, [Text])
      in case matches' of
        (_, _, _, [expected, actual]) -> TypeMismatch expected actual
        _ -> UnknownTypeError msg

parseScopeError :: Text -> ScopeErrorInfo
parseScopeError msg =
  -- Extract variable name from "Not in scope: 'varName'"
  let pattern = "Not in scope: \8216([^\8217]+)\8217" :: Text -- Smart quotes
      matches = (msg =~ pattern) :: (Text, Text, Text, [Text])
  in case matches of
    (_, _, _, [varName]) -> VariableNotInScope varName
    _ ->
      let pattern' = "Not in scope: '([^']+)'" :: Text
          matches' = (msg =~ pattern') :: (Text, Text, Text, [Text])
      in case matches' of
        (_, _, _, [varName]) -> VariableNotInScope varName
        _ -> UnknownScopeError msg
