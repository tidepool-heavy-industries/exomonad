module Main where

import DM.State
import DM.Loop
import DM.Tools (DMEvent(..))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
  putStrLn "Tidepool DM - Type-safe LLM Agent Loop"
  putStrLn "======================================="
  putStrLn ""
  
  let world = setupExampleWorld
  
  putStrLn $ "World initialized with:"
  putStrLn $ "  - " <> show (HM.size (factions world)) <> " factions"
  putStrLn $ "  - " <> show (HM.size (npcs world)) <> " NPCs"
  putStrLn $ "  - " <> show (HM.size (locations world)) <> " locations"
  putStrLn $ "  - " <> show (HM.size (clocks world)) <> " clocks"
  putStrLn $ "  - " <> show (length (threads world)) <> " threads"
  putStrLn ""
  
  runDMGame world handleEvent
  where
    handleEvent :: DMEvent -> IO ()
    handleEvent (DMThought t) = TIO.putStrLn $ "[DM thinks] " <> t
    handleEvent (NPCSpoke nid t) = TIO.putStrLn $ "[NPC " <> unNpcId nid <> "] " <> t
    handleEvent (PlayerAsked q) = TIO.putStrLn $ "[Asks player] " <> q
    handleEvent (RandomChoice label idx) = 
      TIO.putStrLn $ "[Random] chose " <> label <> " (index " <> showT idx <> ")"
    
    showT :: Int -> Text
    showT i = error "TODO: showT - convert Int to Text"

setupExampleWorld :: WorldState
setupExampleWorld = error "TODO: setupExampleWorld - create Blades in the Dark style factions, NPCs, locations, clocks, threads"
