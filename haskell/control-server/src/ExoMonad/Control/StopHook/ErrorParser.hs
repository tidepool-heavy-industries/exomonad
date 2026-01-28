{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Control.StopHook.ErrorParser
  ( parseGHCOutput
  , classifyError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))
import Text.Read (readMaybe)

import ExoMonad.Control.StopHook.Types

-- | Parse GHC error output into structured errors
parseGHCOutput :: Text -> [GHCError]
parseGHCOutput output = 
  let lines' = T.lines output
  in parseLines lines' Nothing

parseLines :: [Text] -> Maybe GHCError -> [GHCError]
parseLines [] Nothing = []
parseLines [] (Just current) = [finalizeError current]
parseLines (l:ls) Nothing = case parseHeader l of
  Just newErr -> parseLines ls (Just newErr)
  Nothing -> parseLines ls Nothing -- Skip non-error lines at start
parseLines (l:ls) (Just current) = case parseHeader l of
  Just newErr -> finalizeError current : parseLines ls (Just newErr)
  Nothing -> 
    -- Append line to current error message
    let updated = current { geMessage = geMessage current <> "\n" <> l }
    in parseLines ls (Just updated)

finalizeError :: GHCError -> GHCError
finalizeError err = err { geErrorType = classifyError (geMessage err) }

-- | Parse a single error line header
-- Format: path/to/File.hs:line:col: error: message
-- or: path/to/File.hs:line:col: warning: message
parseHeader :: Text -> Maybe GHCError
parseHeader line =
  let pattern = "^([^:]+):([0-9]+):([0-9]+): (error|warning): (.*)$" :: Text
      matches = (line =~ pattern) :: (Text, Text, Text, [Text])
  in case matches of
    (_, _, _, [file, lineNum, col, severity, msg]) -> 
      case (readMaybe (T.unpack lineNum), readMaybe (T.unpack col)) of
        (Just ln, Just c) -> Just $ GHCError
          { geFile = T.unpack file
          , geLine = ln
          , geColumn = c
          , geMessage = msg
          , geErrorType = OtherError "" -- Placeholder, classified later
          , geSeverity = if severity == "error" then ErrorSeverity else WarningSeverity
          }
        _ -> Nothing
    _ -> Nothing

-- | Classify error by pattern matching on message
classifyError :: Text -> GHCErrorType
classifyError msg
  | "Couldn't match type" `T.isInfixOf` msg || "Couldn't match expected type" `T.isInfixOf` msg = 
      TypeError $ parseTypeMismatch msg
  | "Not in scope" `T.isInfixOf` msg || "not in scope" `T.isInfixOf` msg = ScopeError $ parseScopeError msg
  | "parse error" `T.isInfixOf` msg = ParseError msg
  | "Ambiguous type variable" `T.isInfixOf` msg = TypeError $ AmbiguousType msg
  | "No instance for" `T.isInfixOf` msg = TypeError $ MissingInstance msg
  | otherwise = OtherError msg

parseTypeMismatch :: Text -> TypeErrorInfo
parseTypeMismatch msg = 
  -- Try to extract expected/actual. This is fuzzy because GHC output varies.
  -- Pattern 1: Couldn't match type 'X' with 'Y'
  -- Pattern 2: Couldn't match expected type 'X' with actual type 'Y'
  -- \x2018 = left single quote, \x2019 = right single quote
  
  let pattern1 = "Couldn't match type [\x2018']([^\x2019']+)[\x2019'] with [\x2018']([^\x2019']+)[\x2019']" :: Text
      matches1 = (msg =~ pattern1) :: (Text, Text, Text, [Text])
      
      pattern2 = "Couldn't match expected type [\x2018']([^\x2019']+)[\x2019'] with actual type [\x2018']([^\x2019']+)[\x2019']" :: Text
      matches2 = (msg =~ pattern2) :: (Text, Text, Text, [Text])
  in case (matches1, matches2) of
    ((_, _, _, [actual, expected]), _) -> TypeMismatch expected actual
    (_, (_, _, _, [expected, actual])) -> TypeMismatch expected actual
    _ -> UnknownTypeError msg

parseScopeError :: Text -> ScopeErrorInfo
parseScopeError msg = 
  -- Extract variable name from "Not in scope: 'varName'"
  -- Handles smart quotes \x2018 (start) and \x2019 (end)
  let pattern = "Not in scope: [\x2018']([^\x2019']+)[\x2019']" :: Text
      matches = (msg =~ pattern) :: (Text, Text, Text, [Text])
      
      -- Handle unquoted: "Variable not in scope: varName :: Type"
      pattern2 = "Variable not in scope: ([^: \n]+)" :: Text
      matches2 = (msg =~ pattern2) :: (Text, Text, Text, [Text])
  in case (matches, matches2) of
    ((_, _, _, [varName]), _) -> VariableNotInScope varName
    (_, (_, _, _, [varName])) -> VariableNotInScope varName
    _ -> UnknownScopeError msg