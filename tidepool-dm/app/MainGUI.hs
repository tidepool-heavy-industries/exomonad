-- | Main entry point for the Tidepool DM GUI
--
-- This runs the GUI connected to the real DM game loop with DB persistence.
-- Game selection happens in the terminal, then the GUI opens.
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (replicateM)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)
import Effectful (runEff)

import DM.State (WorldState(..), initialWorld, DicePool(..))
import DM.GUI.Widgets.Events (formatEvent)
import Tidepool.Effect (runRandom, randomInt)
import DM.Loop (gameLoopWithGUIAndDB)
import DM.GUI.App (dmGUISetup, defaultDMGUIConfig)
import Tidepool.GUI.Core (newGUIBridge, logInfo, addNarrative)
import Tidepool.GUI.Server (startServer, defaultServerConfig, ServerConfig(..))
import qualified Tidepool.Storage as Storage

-- | Database file location
dbFilePath :: FilePath
dbFilePath = "tidepool-games.db"

main :: IO ()
main = do
  putStrLn "Tidepool DM GUI - Blades in the Dark"
  putStrLn "====================================="
  putStrLn ""

  -- Open database and run game selection
  Storage.withGameDB dbFilePath $ \conn -> do
    -- Show game menu and get choice
    (gameId, world, mCursor) <- gameMenu conn

    putStrLn ""
    putStrLn "Starting GUI server..."
    putStrLn "Open http://localhost:8023 in your browser"
    putStrLn ""

    -- Create the GUI bridge with loaded world state
    bridge <- newGUIBridge world

    -- Event handler: display events in GUI narrative + debug log
    let handleEvent event = do
          -- Always log to debug panel
          logInfo bridge (T.pack $ show event)
          -- Add formatted events to narrative (for styled display)
          for_ (formatEvent event) (addNarrative bridge)

    -- Start game loop in background thread (with DB persistence)
    _ <- forkIO $ gameLoopWithGUIAndDB conn gameId mCursor bridge handleEvent

    -- Configure and start the GUI server (blocks main thread)
    let config = defaultServerConfig
          { scTitle = "Tidepool DM - Blades in the Dark"
          }

    startServer config (dmGUISetup defaultDMGUIConfig bridge)

-- | Game selection menu
-- Returns (GameId, WorldState, Maybe compression cursor)
gameMenu :: Storage.Connection -> IO (Storage.GameId, WorldState, Maybe Int)
gameMenu conn = do
  games <- Storage.listGames conn
  if null games
    then do
      putStrLn "No saved games found. Starting new game..."
      newGame conn
    else do
      putStrLn "Saved Games:"
      putStrLn "────────────"
      mapM_ printGame (zip [1..] games)
      putStrLn ""
      putStrLn "[n] New game"
      putStrLn "[d] Delete a game"
      putStrLn ""
      putStr "Choice: "
      hFlush stdout
      choice <- getLine
      case choice of
        "n" -> newGame conn
        "d" -> deleteGameMenu conn >> gameMenu conn
        _ -> case reads choice :: [(Int, String)] of
          [(n, "")] | n >= 1 && n <= length games ->
            loadExistingGame conn (games !! (n - 1))
          _ -> do
            putStrLn "Invalid choice, try again."
            gameMenu conn
  where
    printGame :: (Int, (Storage.GameId, Maybe Text, Text)) -> IO ()
    printGame (n, (gid, mName, updatedAt)) = do
      let name = fromMaybe (Storage.gameIdText gid) mName
          -- Show just the date portion of ISO8601 timestamp
          dateStr = T.take 10 updatedAt
      TIO.putStrLn $ "[" <> showT n <> "] " <> name <> " (last played: " <> dateStr <> ")"

-- | Create a new game
newGame :: Storage.Connection -> IO (Storage.GameId, WorldState, Maybe Int)
newGame conn = do
  putStr "Game name (or enter for default): "
  hFlush stdout
  nameInput <- getLine
  let mName = if null nameInput then Nothing else Just (T.pack nameInput)

  -- Generate starting dice pool using Random effect
  startingDice <- generateStartingDice
  let world = initialWorld { dicePool = DicePool startingDice }

  gameId <- Storage.createGame conn mName world
  TIO.putStrLn $ "Created game: " <> Storage.gameIdText gameId
  return (gameId, world, Nothing)

-- | Load an existing game
loadExistingGame
  :: Storage.Connection
  -> (Storage.GameId, Maybe Text, Text)
  -> IO (Storage.GameId, WorldState, Maybe Int)
loadExistingGame conn (gameId, mName, _) = do
  TIO.putStrLn $ "Loading: " <> fromMaybe (Storage.gameIdText gameId) mName
  result <- Storage.loadGame conn gameId
  case result of
    Nothing -> do
      putStrLn "Error: Could not load game state. Starting fresh."
      startingDice <- generateStartingDice
      let world = initialWorld { dicePool = DicePool startingDice }
      return (gameId, world, Nothing)
    Just (world, mCursor, _mSummary) -> do
      msgCount <- Storage.getMessageCount conn gameId
      putStrLn $ "Loaded game with " <> show msgCount <> " messages in history."
      return (gameId, world, mCursor)

-- | Delete a game
deleteGameMenu :: Storage.Connection -> IO ()
deleteGameMenu conn = do
  games <- Storage.listGames conn
  if null games
    then putStrLn "No games to delete."
    else do
      putStrLn "\nWhich game to delete?"
      mapM_ printGame (zip [1..] games)
      putStr "Choice (or 'c' to cancel): "
      hFlush stdout
      choice <- getLine
      case choice of
        "c" -> return ()
        _ -> case reads choice :: [(Int, String)] of
          [(n, "")] | n >= 1 && n <= length games -> do
            let (gameId, mName, _) = games !! (n - 1)
            TIO.putStrLn $ "Deleting: " <> fromMaybe (Storage.gameIdText gameId) mName
            Storage.deleteGame conn gameId
            putStrLn "Deleted."
          _ -> putStrLn "Invalid choice."
  where
    printGame :: (Int, (Storage.GameId, Maybe Text, Text)) -> IO ()
    printGame (n, (gid, mName, _)) = do
      let name = fromMaybe (Storage.gameIdText gid) mName
      TIO.putStrLn $ "[" <> showT n <> "] " <> name

showT :: Int -> Text
showT = T.pack . show

-- | Generate a starting dice pool (4d6) using the Random effect
generateStartingDice :: IO [Int]
generateStartingDice = runEff . runRandom $
  replicateM 4 (randomInt 1 6)
