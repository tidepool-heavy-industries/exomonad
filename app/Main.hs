module Main where

import DM.State
import DM.Loop
import DM.Tools (DMEvent(..))
import qualified Tidepool.Storage as Storage
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)

-- | Database file location
dbFilePath :: FilePath
dbFilePath = "tidepool-games.db"

main :: IO ()
main = do
  putStrLn "Tidepool DM - Type-safe LLM Agent Loop"
  putStrLn "======================================="
  putStrLn ""

  -- Open database and run game selection
  Storage.withGameDB dbFilePath $ \conn -> do
    -- Show game menu and get choice
    (gameId, world, mCursor) <- gameMenu conn

    printWorldInfo world

    -- Run game with DB persistence
    _ <- runDMGameWithDB conn gameId mCursor world handleEvent
    putStrLn "Game saved to database."

  where
    printWorldInfo world = do
      putStrLn $ "World initialized with:"
      putStrLn $ "  - " <> show (HM.size world.factions) <> " factions"
      putStrLn $ "  - " <> show (HM.size world.npcs) <> " NPCs"
      putStrLn $ "  - " <> show (HM.size world.locations) <> " locations"
      putStrLn $ "  - " <> show (HM.size world.clocks) <> " clocks"
      putStrLn $ "  - " <> show (length world.threads) <> " threads"
      TIO.putStrLn $ "  - Dice pool: " <> formatDice world.dicePool.poolDice
      putStrLn ""

    formatDice dice = T.intercalate " " [dieChar d | d <- dice]
    dieChar d = case d of
      1 -> "⚀"; 2 -> "⚁"; 3 -> "⚂"; 4 -> "⚃"; 5 -> "⚄"; 6 -> "⚅"; _ -> "?"

-- | Event handler for game events
handleEvent :: DMEvent -> IO ()
handleEvent (RandomChoice label idx) =
  TIO.putStrLn $ "[Random] chose " <> label <> " (index " <> showT idx <> ")"
handleEvent (DieSpent dieVal outcomeTier narrativeText) = do
  TIO.putStrLn $ "\n🎲 Die spent: " <> showT dieVal <> " → " <> T.pack (show outcomeTier)
  TIO.putStrLn $ narrativeText
handleEvent (ClockCompleted clockId clockName _consequence) =
  TIO.putStrLn $ "[Clock filled] ⏰ " <> clockName <> " (" <> clockId <> ") - consequence triggered!"
handleEvent (SceneCompressed summary) =
  TIO.putStrLn $ "[Scene compressed] " <> summary
handleEvent (MoodTransition toolName fromMood toMood) =
  TIO.putStrLn $ "\n⚡ [" <> toolName <> "] " <> fromMood <> " → " <> toMood
-- State change events
handleEvent (StressChanged from to reason) = do
  let delta = to - from
      arrow = if delta > 0 then "+" else ""
  TIO.putStrLn $ "📊 Stress: " <> showT from <> " → " <> showT to <>
    " (" <> arrow <> showT delta <> ") — " <> reason
handleEvent (TraumaTriggered trauma trigger breakingPoint) = do
  TIO.putStrLn $ "\n💔 ═══════════════════════════════════════"
  TIO.putStrLn $ "   TRAUMA: " <> T.toUpper (unTrauma trauma)
  TIO.putStrLn $ "   " <> breakingPoint
  TIO.putStrLn $ "   " <> trigger
  TIO.putStrLn $ "═══════════════════════════════════════\n"
  where unTrauma (Trauma t) = t
handleEvent (HeatChanged from to reason) = do
  let delta = to - from
      arrow = if delta > 0 then "+" else ""
  TIO.putStrLn $ "🔥 Heat: " <> showT from <> " → " <> showT to <>
    " (" <> arrow <> showT delta <> ") — " <> reason
handleEvent (WantedChanged from to reason) =
  TIO.putStrLn $ "⚠️ Wanted: " <> showT from <> " → " <> showT to <> " — " <> reason
handleEvent (CoinChanged from to reason) = do
  let delta = to - from
      arrow = if delta > 0 then "+" else ""
  TIO.putStrLn $ "💰 Coin: " <> showT from <> " → " <> showT to <>
    " (" <> arrow <> showT delta <> ") — " <> reason
handleEvent (DicePoolDepleted context) =
  TIO.putStrLn $ "\n⚀ DICE POOL EMPTY — " <> context
handleEvent (BargainOffered context canRetreat) = do
  TIO.putStrLn $ "\n⚖️ BARGAIN REQUIRED"
  TIO.putStrLn $ "   " <> context
  TIO.putStrLn $ if canRetreat
    then "   You can retreat to safety, or make a deal..."
    else "   No escape. You must bargain or collapse."
handleEvent (ClockAdvanced clockId clockName oldFilled newFilled total) = do
  let ticks = newFilled - oldFilled
  TIO.putStrLn $ "⏰ " <> clockName <> ": " <> showT newFilled <> "/" <> showT total <>
    " [+" <> showT ticks <> "] (" <> clockId <> ")"
handleEvent (NarrativeAdded _) = pure ()  -- Handled elsewhere

showT :: Int -> Text
showT = T.pack . show

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
      world = setupExampleWorld

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
      return (gameId, setupExampleWorld, Nothing)
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

setupExampleWorld :: WorldState
setupExampleWorld = initialWorld
  { player = initialPlayer { coin = 4, stress = 2 }
  , dicePool = DicePool [6, 4, 3, 2]  -- Starting dice pool
  , factions = HM.fromList
      -- The Bluecoats - corrupt city watch
      [ (FactionId "bluecoats", Faction
          { factionName = "The Bluecoats"
          , factionAttitude = Wary
          , factionGoals =
              [ Goal (GoalId "maintain-order") "Maintain the appearance of order in Crow's Foot" Pursuing
              , Goal (GoalId "collect-bribes") "Collect tribute from local businesses" Pursuing
              ]
          , factionResources = ResourcePool { gold = 50, soldiers = 20, influence = 3, specialResources = [("Patrol routes", 1)] }
          , factionSecrets = [Secret "Captain Helker takes bribes from the Lampblacks" []]
          , factionKnownFacts = [Fact "The Bluecoats are stretched thin after the Unity War" "Common knowledge"]
          })

      -- The Lampblacks - ex-Gondoliers turned gang
      , (FactionId "lampblacks", Faction
          { factionName = "The Lampblacks"
          , factionAttitude = Neutral
          , factionGoals =
              [ Goal (GoalId "control-coal") "Control the coal trade in Crow's Foot" Pursuing
              , Goal (GoalId "destroy-crows") "Drive the Red Sashes out of the district" Pursuing
              ]
          , factionResources = ResourcePool { gold = 30, soldiers = 12, influence = 2, specialResources = [("Coal warehouses", 2), ("Gondola contacts", 1)] }
          , factionSecrets = [Secret "Bazso Baz murdered his predecessor to take control" [Right (NpcId "bazso")]]
          , factionKnownFacts = [Fact "The Lampblacks control the coal distribution in Crow's Foot" "Street knowledge"]
          })

      -- The Red Sashes - weapon smugglers with noble pretensions
      , (FactionId "red-sashes", Faction
          { factionName = "The Red Sashes"
          , factionAttitude = Hostile
          , factionGoals =
              [ Goal (GoalId "weapons-monopoly") "Monopolize the weapons trade" Pursuing
              , Goal (GoalId "eliminate-lampblacks") "Eliminate the Lampblacks" Pursuing
              ]
          , factionResources = ResourcePool { gold = 45, soldiers = 15, influence = 2, specialResources = [("Iruvian blades", 3), ("Sword-fighting training", 1)] }
          , factionSecrets = [Secret "The Red Sashes smuggle ghost essence for the Spirit Wardens" [Left (FactionId "spirit-wardens")]]
          , factionKnownFacts = [Fact "The Red Sashes claim noble Iruvian heritage" "Common knowledge"]
          })

      -- The Crows - information brokers (decimated, power vacuum)
      , (FactionId "crows", Faction
          { factionName = "The Crows"
          , factionAttitude = Favorable
          , factionGoals =
              [ Goal (GoalId "rebuild") "Rebuild after Roric's assassination" (Blocked "Leadership vacuum")
              , Goal (GoalId "find-killer") "Find who killed Roric" Pursuing
              ]
          , factionResources = ResourcePool { gold = 15, soldiers = 4, influence = 1, specialResources = [("Spy network", 2)] }
          , factionSecrets = [Secret "Lyssa ordered Roric's death to seize power" [Right (NpcId "lyssa")]]
          , factionKnownFacts = [Fact "Roric was assassinated last week, the Crows are in chaos" "Street knowledge"]
          })

      -- Spirit Wardens - handle the dead, politically powerful
      , (FactionId "spirit-wardens", Faction
          { factionName = "Spirit Wardens"
          , factionAttitude = Neutral
          , factionGoals =
              [ Goal (GoalId "contain-dead") "Contain all ghosts and properly dispose of corpses" Pursuing
              ]
          , factionResources = ResourcePool { gold = 100, soldiers = 30, influence = 5, specialResources = [("Crematorium access", 1), ("Spirit bottles", 10)] }
          , factionSecrets = []
          , factionKnownFacts = [Fact "The Spirit Wardens answer only to the Emperor" "Common knowledge"]
          })
      ]

  , npcs = HM.fromList
      -- Bazso Baz - leader of the Lampblacks
      [ (NpcId "bazso", Npc
          { npcName = "Bazso Baz"
          , npcFaction = Just (FactionId "lampblacks")
          , npcDisposition = Friendly
          , npcWants =
              [ Want "Find someone to do a dangerous job against the Red Sashes" High
              , Want "Impress a noble lady he's courting" Medium
              ]
          , npcFears =
              [ Fear "His past as a murderer being exposed" Severe
              , Fear "The Lampblacks falling to the Red Sashes" Existential
              ]
          , npcKnows = [Secret "There's a secret tunnel under the old coal warehouse" []]
          , npcLocation = Just (LocationId "coal-warehouse")
          , npcVoiceNotes = "Charismatic, working-class accent, uses 'friend' a lot. Dangerous beneath the charm."
          })

      -- Mylera Klev - leader of the Red Sashes
      , (NpcId "mylera", Npc
          { npcName = "Mylera Klev"
          , npcFaction = Just (FactionId "red-sashes")
          , npcDisposition = Suspicious
          , npcWants =
              [ Want "Destroy Bazso Baz personally" High
              , Want "Prove the Red Sashes' noble lineage" Medium
              ]
          , npcFears =
              [ Fear "Being seen as common criminals" Moderate
              , Fear "The ghost smuggling operation being discovered" Severe
              ]
          , npcKnows = [Secret "The Spirit Wardens are corrupt and buy ghost essence" []]
          , npcLocation = Just (LocationId "sword-hall")
          , npcVoiceNotes = "Aristocratic Iruvian accent. Cold, precise, speaks of 'honor' while planning murders."
          })

      -- Lyssa - new leader of the Crows
      , (NpcId "lyssa", Npc
          { npcName = "Lyssa"
          , npcFaction = Just (FactionId "crows")
          , npcDisposition = DispNeutral
          , npcWants =
              [ Want "Consolidate power before others discover her role" UrgencyDesperate
              , Want "Find competent outsiders to do her dirty work" High
              ]
          , npcFears =
              [ Fear "Being exposed as Roric's killer" Existential
              , Fear "The old Crows turning against her" Severe
              ]
          , npcKnows = [Secret "Roric had discovered evidence of Spirit Warden corruption" []]
          , npcLocation = Just (LocationId "crows-nest")
          , npcVoiceNotes = "Whispers even in private. Paranoid pauses. Tests everyone constantly."
          })

      -- Captain Helker - corrupt Bluecoat
      , (NpcId "helker", Npc
          { npcName = "Captain Helker"
          , npcFaction = Just (FactionId "bluecoats")
          , npcDisposition = DispNeutral
          , npcWants =
              [ Want "Maintain his comfortable corruption" Medium
              , Want "Retire before things get too dangerous" Low
              ]
          , npcFears =
              [ Fear "The gang war spilling into the streets" Moderate
              , Fear "Being investigated by the Inspectors" Severe
              ]
          , npcKnows = [Secret "The Lampblacks pay him 2 coin per week for advance warning" []]
          , npcLocation = Just (LocationId "bluecoat-station")
          , npcVoiceNotes = "Tired, cynical, always mentions how long until retirement. Actually decent at his job."
          })

      -- Nyryx - a ghost trapped in a spirit bottle
      , (NpcId "nyryx", Npc
          { npcName = "Nyryx"
          , npcFaction = Nothing
          , npcDisposition = DispHostile
          , npcWants =
              [ Want "Be released or given a new body" UrgencyDesperate
              , Want "Take revenge on whoever bottled them" High
              ]
          , npcFears =
              [ Fear "Being destroyed by the Spirit Wardens" Existential
              , Fear "Fading away forgotten" Severe
              ]
          , npcKnows = [Secret "Saw who killed Roric - it was Lyssa" []]
          , npcLocation = Just (LocationId "forgotten-gods")
          , npcVoiceNotes = "Echoing whisper. Speaks in fragments. Sometimes lapses into old Akorosi. Bitter but desperate."
          })

      -- Telda - bartender and information broker
      , (NpcId "telda", Npc
          { npcName = "Telda"
          , npcFaction = Nothing
          , npcDisposition = Friendly
          , npcWants =
              [ Want "Keep the Leaky Bucket neutral territory" High
              , Want "Collect interesting secrets" Medium
              ]
          , npcFears =
              [ Fear "The gang war destroying her tavern" Moderate
              , Fear "Someone finding her hidden ledger" Severe
              ]
          , npcKnows = [Secret "Keeps a coded ledger of everyone's debts and secrets" []]
          , npcLocation = Just (LocationId "leaky-bucket")
          , npcVoiceNotes = "Warm but watchful. Laughs easily. Remembers everything. Never takes sides publicly."
          })
      ]

  , locations = HM.fromList
      [ (LocationId "intro", Location
          { locationName = "The Streets of Crow's Foot"
          , locationDescription = "Fog drifts through gas-lit streets. Somewhere a clock tower chimes. The district breathes with old violence and new opportunity."
          , locationControlledBy = Nothing
          , locationFeatures = ["Narrow alleys", "Canal bridges", "Gas-lamps", "Shadows everywhere"]
          })

      , (LocationId "coal-warehouse", Location
          { locationName = "Lampblack Coal Warehouse"
          , locationDescription = "A sprawling brick warehouse reeking of coal dust. The Lampblacks' headquarters."
          , locationControlledBy = Just (FactionId "lampblacks")
          , locationFeatures = ["Hidden basement", "Roof access", "Coal chutes for quick escape", "Armed guards at all hours"]
          })

      , (LocationId "sword-hall", Location
          { locationName = "The Sword Hall"
          , locationDescription = "An elegant fencing academy that serves as Red Sash headquarters. Iruvian tapestries on the walls."
          , locationControlledBy = Just (FactionId "red-sashes")
          , locationFeatures = ["Training floor", "Secret weapon cache", "Rooftop garden", "Noble visitors during day"]
          })

      , (LocationId "crows-nest", Location
          { locationName = "The Crow's Nest"
          , locationDescription = "A ramshackle tower overlooking the district. Once the Crows' proud headquarters, now half-empty."
          , locationControlledBy = Just (FactionId "crows")
          , locationFeatures = ["Observation deck", "Spy-holes into neighboring buildings", "Roric's old office (sealed)", "Messenger birds"]
          })

      , (LocationId "leaky-bucket", Location
          { locationName = "The Leaky Bucket"
          , locationDescription = "A grimy tavern that serves as neutral ground. Everyone drinks here."
          , locationControlledBy = Nothing
          , locationFeatures = ["Back room for private meetings", "Trapdoor to the canals", "Cheap whiskey", "No violence tolerated"]
          })

      , (LocationId "bluecoat-station", Location
          { locationName = "Crow's Foot Watch Station"
          , locationDescription = "A fortified stone building where the local Bluecoats operate."
          , locationControlledBy = Just (FactionId "bluecoats")
          , locationFeatures = ["Holding cells", "Evidence locker", "Captain's office", "Back door for 'discrete' visitors"]
          })

      , (LocationId "forgotten-gods", Location
          { locationName = "Shrine of Forgotten Gods"
          , locationDescription = "An abandoned temple in the undercity. Strange echoes. Spirit activity."
          , locationControlledBy = Nothing
          , locationFeatures = ["Altar to dead gods", "Hidden catacombs", "Ghost activity", "Spirit well (unstable)"]
          })

      , (LocationId "coalridge-canal", Location
          { locationName = "Coalridge Canal"
          , locationDescription = "Dark waters flowing between districts. Gondoliers pole through the fog."
          , locationControlledBy = Nothing
          , locationFeatures = ["Smuggling routes", "Fog cover", "Echo amplification", "Connects to undercity"]
          })
      ]

  , clocks = HM.fromList
      -- Major clocks driving the narrative
      [ (ClockId "gang-war-erupts", Clock
          { clockName = "Gang War Erupts"
          , clockSegments = 6
          , clockFilled = 2
          , clockVisible = True
          , clockType = ThreatClock
          , clockConsequence = Escalate (Escalation "Open warfare in the streets of Crow's Foot" Severe)
          })

      , (ClockId "bluecoat-crackdown", Clock
          { clockName = "Bluecoat Crackdown"
          , clockSegments = 4
          , clockFilled = 0
          , clockVisible = True
          , clockType = ThreatClock
          , clockConsequence = FactionMoves (FactionId "bluecoats") (Attack (Target "all criminal elements"))
          })

      , (ClockId "lyssa-exposed", Clock
          { clockName = "Lyssa's Secret Exposed"
          , clockSegments = 4
          , clockFilled = 0
          , clockVisible = False
          , clockType = GoalClock
          , clockConsequence = RevealSecret (Secret "Lyssa killed Roric to take over the Crows" [])
          })

      , (ClockId "ghost-problem", Clock
          { clockName = "Spirit Well Rupture"
          , clockSegments = 8
          , clockFilled = 3
          , clockVisible = True
          , clockType = ThreatClock
          , clockConsequence = SpawnThread (Thread
              (ThreadId "ghost-plague")
              "Ghosts flood the streets of Crow's Foot"
              [Left (FactionId "spirit-wardens")]
              TensionCritical
              Nothing)
          })
      ]

  , rumors =
      [ Rumor
          { rumorId = RumorId "roric-murder"
          , rumorContent = "Roric didn't die in an accident - he was murdered by someone inside the Crows"
          , rumorSource = OverheardFrom (NpcId "telda")
          , rumorTruthValue = TrueRumor
          , rumorSpread = Tavern
          }
      , Rumor
          { rumorId = RumorId "spirit-warden-corruption"
          , rumorContent = "The Spirit Wardens are buying ghost essence from criminals"
          , rumorSource = FromFaction (FactionId "red-sashes")
          , rumorTruthValue = TrueRumor
          , rumorSpread = Whisper
          }
      , Rumor
          { rumorId = RumorId "bazso-noble-lady"
          , rumorContent = "Bazso Baz is courting a noble lady - some say a Bowmore"
          , rumorSource = PublicKnowledge
          , rumorTruthValue = PartiallyTrue "He is courting someone, but she's not noble - she's a Spirit Warden initiate"
          , rumorSpread = Tavern
          }
      ]

  , threads =
      [ Thread
          { threadId = ThreadId "power-vacuum"
          , threadHook = "With Roric dead, the Crows are weak. Someone will absorb their territory - but who?"
          , threadInvolves = [Left (FactionId "crows"), Left (FactionId "lampblacks"), Left (FactionId "red-sashes")]
          , threadTension = Rising
          , threadDeadline = Just (ClockId "gang-war-erupts")
          }
      , Thread
          { threadId = ThreadId "ghost-witness"
          , threadHook = "A ghost in the undercity saw who killed Roric. But ghosts are unreliable witnesses..."
          , threadInvolves = [Right (NpcId "nyryx"), Right (NpcId "lyssa")]
          , threadTension = Simmering
          , threadDeadline = Just (ClockId "lyssa-exposed")
          }
      , Thread
          { threadId = ThreadId "neutral-ground"
          , threadHook = "The Leaky Bucket is the last neutral space in Crow's Foot. Both sides want to control it."
          , threadInvolves = [Right (NpcId "telda"), Left (FactionId "lampblacks"), Left (FactionId "red-sashes")]
          , threadTension = Rising
          , threadDeadline = Nothing
          }
      ]

  , tone = Tense
  , sessionGoals = ["Establish yourself in Crow's Foot", "Pick a side (or play them against each other)", "Survive the coming storm"]
  }
