{-# LANGUAGE OverloadedStrings #-}

-- | Executable runner for MyTask graph
module Main (main) where

import System.Environment (getArgs)
import Data.Text (pack)

import HumanDrivenDev.MyTaskGraph (MyTaskGraph(..))
import HumanDrivenDev.MyTaskInterpreter (runMyTaskGraph)
import HumanDrivenDev.Types.Core (TopLevelInput(..), Spec(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [description, targetPath] -> do
      let spec = Spec
            { sDescription = pack description
            , sAcceptanceCriteria = []
            , sTargetPath = targetPath
            , sContext = Nothing
            }
          input = TopLevelInput
            { tliSpec = spec
            , tliWorkDir = "."
            }

      -- TODO: Build assembled graph from definitions
      let graph = undefined  -- Replace with: assembleGraph myTaskGraphDef

      result <- runMyTaskGraph graph input
      print result

    _ -> do
      putStrLn "Usage: human-driven-dev <description> <target-path>"
      putStrLn ""
      putStrLn "Example:"
      putStrLn "  human-driven-dev \"Build a URL shortener\" ./src"
