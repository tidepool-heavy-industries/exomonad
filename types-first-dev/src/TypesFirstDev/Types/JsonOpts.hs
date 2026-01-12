{-# LANGUAGE OverloadedStrings #-}

-- | JSON parsing options for types-first-dev types.
-- Separate module for TH staging requirements.
module TypesFirstDev.Types.JsonOpts
  ( jsonOpts
  , stripPrefixSnakeCase
  ) where

import Data.Aeson.TH (defaultOptions, Options(..), fieldLabelModifier)
import Data.Char (toLower, isUpper)

-- | Convert prefixed camelCase to snake_case, stripping prefix.
-- e.g., "sDescription" -> "description", "sTargetPath" -> "target_path"
stripPrefixSnakeCase :: String -> String
stripPrefixSnakeCase = camelToSnake . dropPrefix
  where
    -- Drop lowercase prefix (e.g., "sId" -> "Id", "cnTime" -> "Time")
    dropPrefix s = case dropWhile (not . isUpper) s of
      [] -> s  -- No uppercase found, keep original
      rest -> rest
    -- Convert CamelCase to snake_case
    camelToSnake [] = []
    camelToSnake (x:xs) = toLower x : go xs
      where
        go [] = []
        go (c:cs)
          | isUpper c = '_' : toLower c : go cs
          | otherwise = c : go cs

-- | JSON options that strip prefix and convert to snake_case
jsonOpts :: Options
jsonOpts = defaultOptions { fieldLabelModifier = stripPrefixSnakeCase }
