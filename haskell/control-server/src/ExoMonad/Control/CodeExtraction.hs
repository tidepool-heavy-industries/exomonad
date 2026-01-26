{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ExoMonad.Control.CodeExtraction
  ( readCodeAtRange
  ) where

import Control.Exception (try, IOException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ExoMonad.Effect.LSP (Range(..), Position(..))

-- | Read code from a file starting at a given range.
-- Stops at:
-- 1. Section dividers (═══ or ---)
-- 2. Haddock comments for new definitions (-- |)
-- 3. Two consecutive blank lines
-- 4. Next top-level definition (starts with lowercase at column 0)
-- 5. Maximum 30 lines (hard limit)
readCodeAtRange :: FilePath -> Range -> IO Text
readCodeAtRange path range = do
  contentOrErr <- try $ TIO.readFile path :: IO (Either IOException Text)
  case contentOrErr of
    Left _ -> pure ""
    Right content -> do
      let allLines = T.lines content
          startLine = range.rangeStart.posLine
          -- Drop lines before start
          relevantLines = drop startLine allLines
          
          -- Take up to 30 lines maximum
          limitedLines = take 30 relevantLines
          
          -- Apply stopping conditions
          extracted = takeUntilBoundary limitedLines
          
      pure $ T.unlines extracted

-- | Take lines until a boundary condition is met.
takeUntilBoundary :: [Text] -> [Text]
takeUntilBoundary [] = []
takeUntilBoundary (first:rest) = first : go rest (getName first) False
  where
    go :: [Text] -> Maybe Text -> Bool -> [Text]
    go [] _ _ = []
    go (l:ls) currentName lastWasBlank
      -- 1. Section dividers
      | "═══" `T.isInfixOf` l = []
      | "---" `T.isInfixOf` l = []
      
      -- 2. Haddock comments for new definitions (-- |)
      | "-- |" `T.isPrefixOf` (T.stripStart l) = []
      
      -- 3. Two consecutive blank lines
      | T.null (T.strip l) && lastWasBlank = []
      
      -- 4. Next top-level definition
      | isTopLevelDef l = 
          case (currentName, getName l) of
            (Nothing, Just newName) -> l : go ls (Just newName) False -- First def after comments
            (Just n1, Just n2) 
              | n1 == n2 -> l : go ls currentName False -- Same function
              | otherwise -> [] -- Different function
            _ -> l : go ls currentName False -- Should not happen for top level, but safe fallback
      
      -- Continue
      | otherwise = l : go ls currentName (T.null (T.strip l))

    isTopLevelDef :: Text -> Bool
    isTopLevelDef t
      | T.null t = False
      | " " `T.isPrefixOf` t = False -- Indented
      | "--" `T.isPrefixOf` t = False -- Comment
      | "{" `T.isPrefixOf` t = False -- Pragma/Module block
      | "}" `T.isPrefixOf` t = False
      | otherwise = case T.uncons t of
          Just (c, _) -> c >= 'a' && c <= 'z'
          Nothing -> False

    getName :: Text -> Maybe Text
    getName t
      | isTopLevelDef t = Just $ T.takeWhile (\c -> c /= ' ' && c /= ':' && c /= '=') t
      | otherwise = Nothing
