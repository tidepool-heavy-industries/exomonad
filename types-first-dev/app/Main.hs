-- | Main entry point for types-first-dev.
--
-- DEPRECATED: This executable is deprecated. The types-first-dev system now
-- uses the actor runtime in tidepool-actor with the Hybrid graph definitions
-- and handlers in Handlers/Hybrid.
--
-- This stub is kept only for build compatibility.
module Main where

main :: IO ()
main = do
  putStrLn "types-first-dev: This executable is deprecated."
  putStrLn "Use the actor runtime instead (tidepool-actor)."
  putStrLn ""
  putStrLn "The hybrid graph and handlers are in:"
  putStrLn "  - TypesFirstDev.Graph.Hybrid (graph definitions)"
  putStrLn "  - TypesFirstDev.Handlers.Hybrid (handler implementations)"
