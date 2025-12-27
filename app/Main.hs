module Main where

import DM.State
import DM.Loop
import DM.Tools (DMEvent(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
  putStrLn "Tidepool DM - Type-safe LLM Agent Loop"
  putStrLn "======================================="
  putStrLn ""
  
  let world = setupExampleWorld
  
  putStrLn $ "World initialized with:"
  putStrLn $ "  - " <> show (HM.size world.factions) <> " factions"
  putStrLn $ "  - " <> show (HM.size world.npcs) <> " NPCs"
  putStrLn $ "  - " <> show (HM.size world.locations) <> " locations"
  putStrLn $ "  - " <> show (HM.size world.clocks) <> " clocks"
  putStrLn $ "  - " <> show (length world.threads) <> " threads"
  putStrLn ""
  
  runDMGame world handleEvent
  where
    handleEvent :: DMEvent -> IO ()
    handleEvent (DMThought t) = TIO.putStrLn $ "[DM thinks] " <> t
    handleEvent (NPCSpoke nid t) = TIO.putStrLn $ "[NPC " <> nid.unNpcId <> "] " <> t
    handleEvent (PlayerAsked q) = TIO.putStrLn $ "[Asks player] " <> q
    handleEvent (RandomChoice label idx) =
      TIO.putStrLn $ "[Random] chose " <> label <> " (index " <> showT idx <> ")"
    handleEvent (ClockCompleted clockId clockName _consequence) =
      TIO.putStrLn $ "[Clock filled] ⏰ " <> clockName <> " (" <> clockId <> ") - consequence triggered!"
    handleEvent (SceneCompressed summary) =
      TIO.putStrLn $ "[Scene compressed] " <> summary

    showT :: Int -> Text
    showT = T.pack . show

setupExampleWorld :: WorldState
setupExampleWorld = error "TODO: setupExampleWorld - create Blades in the Dark style factions, NPCs, locations, clocks, threads"
