{-# LANGUAGE OverloadedStrings #-}
-- | Test the tidying agent - redirects to GUI
--
-- The tidying agent is GUI-only. Use:
-- @
-- cabal run tidepool-tidy-gui
-- @
module Main where

main :: IO ()
main = do
  putStrLn "=== Tidying Agent ==="
  putStrLn ""
  putStrLn "The tidying agent is GUI-only."
  putStrLn "Please run: cabal run tidepool-tidy-gui"
  putStrLn ""
