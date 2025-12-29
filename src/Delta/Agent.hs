{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Example Delta Agent
--
-- A personal delta agent that routes natural language to destinations:
-- - Groceries → Habitica checklist
-- - Calendar → Calendar events
-- - Knowledge → Obsidian notes
-- - Dev ideas → GitHub issues
--
-- Example usage:
--
-- > "buy milk and eggs, call Sarah tomorrow at 2pm, learned about effect handlers"
-- >
-- > Proposing:
-- >   milk, eggs → groceries
-- >   call Sarah → calendar (when: tomorrow 2pm)
-- >   effect handlers → knowledge
--
module Delta.Agent
  ( -- * Agent (Tidepool pattern)
    delta
  , DeltaM
  , DeltaExtra
  , DeltaEvent(..)

    -- * Running the Agent (legacy)
  , runDeltaAgent
  , DeltaAgentConfig(..)
  , defaultConfig

    -- * Destinations
  , groceriesDestination
  , calendarDestination
  , knowledgeDestination
  , devIdeasDestination

    -- * Effect Stack
  , AgentEffects
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, object, (.=), ToJSON, FromJSON)
import Control.Monad (forM_, forM, unless)
import Effectful
import GHC.Generics (Generic)

import Tidepool (Agent(..), AgentM, noDispatcher)
import Tidepool.Effect
import Tidepool.Delta
import Tidepool.Effects.Habitica
import Tidepool.Effects.Obsidian
import Tidepool.Effects.GitHub
import Tidepool.Effects.Calendar

-- ══════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ══════════════════════════════════════════════════════════════

-- | The full effect stack for the delta agent (legacy)
-- Note: Log must come after the stub effects (Habitica, etc.) because
-- the stub runners require Log :> es in their context
type AgentEffects =
  '[ LLM
   , State UserContext
   , RequestInput
   , ChatHistory
   , Habitica
   , Obsidian
   , GitHub
   , Calendar
   , Log
   , IOE
   ]

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL AGENT PATTERN
-- ══════════════════════════════════════════════════════════════

-- | Delta's extra effects (external integrations)
type DeltaExtra = '[Habitica, Obsidian, GitHub, Calendar]

-- | The Delta agent monad (with external effects)
type DeltaM = AgentM UserContext DeltaEvent DeltaExtra

-- | Events emitted by Delta
data DeltaEvent
  = DeltaProposed Proposal         -- ^ Proposal shown to user
  | DeltaFeedback Text             -- ^ User feedback received
  | DeltaPatternLearned Text Text  -- ^ Pattern learned (match, destination)
  | DeltaRouted Text Text          -- ^ Item routed (entity, destination)
  | DeltaInfo Text                 -- ^ Informational message
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | The Delta agent
--
-- A personal delta agent that routes natural language to destinations.
-- Uses extra effects for external integrations (Habitica, Obsidian, GitHub, Calendar).
--
-- Usage with tidepoolWith:
--
-- @
-- main = do
--   finalState <- tidepoolWith config runDeltaExtra delta
--   putStrLn "Done"
--   where
--     runDeltaExtra = runHabiticaStub . runObsidianStub . runGitHubStub . runCalendarStub
-- @
delta :: Agent UserContext DeltaEvent DeltaExtra
delta = Agent
  { agentName       = "delta"
  , agentInit       = emptyContext
  , agentRun        = deltaRun
  , agentDispatcher = noDispatcher  -- No mid-turn tools, uses external effects instead
  }

-- | The Delta agent's full lifecycle
--
-- TODO: Integrate with existing DeltaConfig and destinations.
-- For now, this is a minimal stub.
deltaRun :: DeltaM ()
deltaRun = do
  -- Startup
  emit $ DeltaInfo "Delta agent ready. Enter text to capture."

  -- Main loop
  loop

  -- Shutdown
  emit $ DeltaInfo "Delta session complete."
  where
    loop = do
      input <- requestText "> "
      case T.toLower (T.strip input) of
        "quit" -> pure ()
        "exit" -> pure ()
        "" -> loop
        _ -> do
          -- Placeholder - full implementation would use runDeltaWithFeedback
          emit $ DeltaInfo $ "Would route: " <> input
          loop

-- ══════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ══════════════════════════════════════════════════════════════

data DeltaAgentConfig = DeltaAgentConfig
  { dacLLMConfig     :: LLMConfig
  , dacLogLevel      :: LogLevel
  , dacGroceryTodo   :: Text      -- Name of the Habitica todo for groceries
  , dacDefaultRepo   :: Text      -- Default GitHub repo for dev ideas
  }

defaultConfig :: Text -> DeltaAgentConfig
defaultConfig apiKey = DeltaAgentConfig
  { dacLLMConfig = LLMConfig
      { llmApiKey = apiKey
      , llmModel = "claude-sonnet-4-20250514"
      , llmMaxTokens = 4096
      , llmThinkingBudget = Nothing
      }
  , dacLogLevel = Info
  , dacGroceryTodo = "Groceries"
  , dacDefaultRepo = "myuser/myrepo"
  }

-- ══════════════════════════════════════════════════════════════
-- DESTINATIONS
-- ══════════════════════════════════════════════════════════════

-- | Groceries → Habitica checklist items
groceriesDestination :: Text -> Destination AgentEffects
groceriesDestination groceryTodoName = Destination
  { destName = "groceries"
  , destDescription = "Food items, household supplies, things to buy at the store"
  , destFormatPrompt = "Just the item name, lowercase, no quantities"
  , destMatchExisting = True
  , destExecute = \_ctx _originalInput entities -> do
      -- Find the groceries todo
      todos <- fetchTodos
      case filter (\t -> t.todoTitle == groceryTodoName) todos of
        [] -> do
          -- Create the todo if it doesn't exist
          tid <- createTodo groceryTodoName
          addItems tid entities
        (todo:_) -> addItems todo.todoId entities
  }
  where
    addItems :: TodoId -> [Text] -> Eff AgentEffects [DeltaResult]
    addItems (TodoId tidText) entities = do
      forM entities $ \entity -> do
        itemId <- addChecklistItem (TodoId tidText) (T.toLower entity)
        pure DeltaResult
          { drEntity = entity
          , drDestination = "groceries"
          , drAsType = "checklist_item"
          , drRef = Just $ tidText <> "/" <> itemId
          }

-- | Calendar → events with times
calendarDestination :: Destination AgentEffects
calendarDestination = Destination
  { destName = "calendar"
  , destDescription = "Events, meetings, calls with specific times or dates"
  , destFormatPrompt = "Event title. Extract date/time from context."
  , destMatchExisting = False
  , destExecute = \_ctx _originalInput entities -> do
      -- For now, just create events with placeholder times
      -- In real impl, would parse dates from originalInput
      forM entities $ \entity -> do
        -- TODO: parse actual date/time from entity or context
        -- For stub, this will error anyway
        today <- error "Calendar: date parsing not implemented"
        noon <- error "Calendar: time parsing not implemented"
        (EventId eid) <- createEvent entity today noon Nothing
        pure DeltaResult
          { drEntity = entity
          , drDestination = "calendar"
          , drAsType = "event"
          , drRef = Just eid
          }
  }

-- | Knowledge → Obsidian notes
knowledgeDestination :: Destination AgentEffects
knowledgeDestination = Destination
  { destName = "knowledge"
  , destDescription = "Facts, learnings, insights worth remembering. Technical concepts, ideas, notes."
  , destFormatPrompt = "Full context. Determine the topic/category. Write as a note with links to related concepts."
  , destMatchExisting = False
  , destExecute = \_ctx originalInput entities -> do
      forM entities $ \entity -> do
        -- Determine topic from entity (in real impl, LLM would do this)
        let topic = inferTopic entity
            pagePath = PagePath $ topic <> ".md"
            (PagePath pagePathText) = pagePath
            noteContent = "\n\n## " <> entity <> "\n\n" <> originalInput

        -- Append to existing page or create new
        pages <- listPages
        if pagePath `elem` pages
          then appendToPage pagePath noteContent
          else createPage pagePath ("# " <> topic <> noteContent)

        pure DeltaResult
          { drEntity = entity
          , drDestination = "knowledge"
          , drAsType = "note"
          , drRef = Just pagePathText
          }
  }
  where
    -- Naive topic inference - real impl would use LLM
    inferTopic :: Text -> Text
    inferTopic entity
      | any (`T.isInfixOf` T.toLower entity) ["haskell", "ghc", "cabal"] = "Haskell"
      | any (`T.isInfixOf` T.toLower entity) ["rust", "cargo", "crate"] = "Rust"
      | any (`T.isInfixOf` T.toLower entity) ["effect", "monad", "functor"] = "FP-Concepts"
      | otherwise = "Notes"

-- | Dev ideas → GitHub issues
devIdeasDestination :: Text -> Destination AgentEffects
devIdeasDestination defaultRepoName = Destination
  { destName = "dev-ideas"
  , destDescription = "Software ideas, feature requests, bugs to file, technical improvements"
  , destFormatPrompt = "Issue title. Body should include context and any relevant details."
  , destMatchExisting = False
  , destExecute = \_ctx originalInput entities -> do
      forM entities $ \entity -> do
        let repo = Repo defaultRepoName
            title = entity
            body = "Captured from: " <> originalInput
            labels = [Label "idea", Label "triage"]

        (IssueUrl url) <- createIssue repo title body labels
        pure DeltaResult
          { drEntity = entity
          , drDestination = "dev-ideas"
          , drAsType = "issue"
          , drRef = Just url
          }
  }

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS
-- ══════════════════════════════════════════════════════════════

proposalSchema :: Value
proposalSchema = object
  [ "type" .= ("object" :: Text)
  , "properties" .= object
      [ "propItems" .= object
          [ "type" .= ("array" :: Text)
          , "items" .= object
              [ "type" .= ("object" :: Text)
              , "properties" .= object
                  [ "piEntity" .= object ["type" .= ("string" :: Text)]
                  , "piDestination" .= object ["type" .= ("string" :: Text)]
                  , "piNeedsClarify" .= object
                      [ "type" .= ("string" :: Text)
                      , "nullable" .= True
                      ]
                  ]
              , "required" .= (["piEntity", "piDestination"] :: [Text])
              , "additionalProperties" .= False
              ]
          ]
      ]
  , "required" .= (["propItems"] :: [Text])
  , "additionalProperties" .= False
  ]

feedbackSchema :: Value
feedbackSchema = object
  [ "type" .= ("object" :: Text)
  , "properties" .= object
      [ "pfApproved" .= object
          [ "type" .= ("array" :: Text)
          , "items" .= object ["type" .= ("string" :: Text)]
          ]
      , "pfCorrections" .= object
          [ "type" .= ("array" :: Text)
          , "items" .= object
              [ "type" .= ("array" :: Text)
              , "items" .= object ["type" .= ("string" :: Text)]
              , "minItems" .= (2 :: Int)
              , "maxItems" .= (2 :: Int)
              ]
          ]
      , "pfClarifications" .= object
          [ "type" .= ("array" :: Text)
          , "items" .= object
              [ "type" .= ("array" :: Text)
              , "minItems" .= (2 :: Int)
              , "maxItems" .= (2 :: Int)
              ]
          ]
      , "pfLearning" .= object
          [ "type" .= ("array" :: Text)
          , "items" .= object
              [ "type" .= ("object" :: Text)
              , "properties" .= object
                  [ "rpMatch" .= object ["type" .= ("string" :: Text)]
                  , "rpDestination" .= object ["type" .= ("string" :: Text)]
                  , "rpSource" .= object ["type" .= ("string" :: Text)]
                  , "rpConfidence" .= object ["type" .= ("number" :: Text)]
                  ]
              , "additionalProperties" .= False
              ]
          ]
      ]
  , "required" .= (["pfApproved", "pfCorrections", "pfClarifications", "pfLearning"] :: [Text])
  , "additionalProperties" .= False
  ]

-- ══════════════════════════════════════════════════════════════
-- PROMPT BUILDERS
-- ══════════════════════════════════════════════════════════════

buildClassifyPrompt :: [Destination es] -> UserContext -> Text
buildClassifyPrompt destinations ctx = T.unlines
  [ "You are a personal assistant that routes input to the right destinations."
  , ""
  , "Available destinations:"
  , T.unlines $ map formatDest destinations
  , ""
  , "Known patterns from past corrections:"
  , T.unlines $ map formatPattern ctx.ucPatterns
  , ""
  , "Extract entities from the user's input and route each to the appropriate destination."
  , "If something needs clarification (like a calendar event needs a time), set piNeedsClarify."
  , ""
  , "Output a JSON object with propItems array."
  ]
  where
    formatDest d = "- " <> d.destName <> ": " <> d.destDescription
    formatPattern p = "- \"" <> p.rpMatch <> "\" → " <> p.rpDestination

buildFeedbackPrompt :: Proposal -> Text
buildFeedbackPrompt proposal = T.unlines
  [ "The user provided feedback on this proposal:"
  , ""
  , "Proposed items:"
  , T.unlines $ map formatItem proposal.propItems
  , ""
  , "Parse the user's feedback to extract:"
  , "- pfApproved: entities the user approved (explicit or implicit)"
  , "- pfCorrections: [(entity, corrected_destination)] for any corrections"
  , "- pfClarifications: [(entity, clarification_value)] for info provided"
  , "- pfLearning: patterns to remember for next time"
  , ""
  , "If user says 'looks good' or similar, approve all."
  , "If user corrects a destination, add to pfCorrections and infer a pattern."
  ]
  where
    formatItem item = "- " <> item.piEntity <> " → " <> item.piDestination
      <> maybe "" (\c -> " (needs: " <> c <> ")") item.piNeedsClarify

-- ══════════════════════════════════════════════════════════════
-- RUNNING THE AGENT
-- ══════════════════════════════════════════════════════════════

-- | Run the delta agent with the given configuration
runDeltaAgent :: DeltaAgentConfig -> IO ()
runDeltaAgent config = do
  let destinations =
        [ groceriesDestination config.dacGroceryTodo
        , calendarDestination
        , knowledgeDestination
        , devIdeasDestination config.dacDefaultRepo
        ]

      deltaConfig = DeltaConfig
        { dcDestinations = destinations
        , dcClassifyPrompt = buildClassifyPrompt
        , dcFeedbackPrompt = buildFeedbackPrompt
        , dcClassifySchema = proposalSchema
        , dcFeedbackSchema = feedbackSchema
        }

      inputHandler = InputHandler
        { ihChoice = \prompt choices -> do
            putStrLn $ T.unpack prompt
            mapM_ (\(i, (label, _)) -> putStrLn $ show i <> ". " <> T.unpack label)
              (zip [1 :: Int ..] choices)
            idx <- readLn
            pure $ snd (choices !! (idx - 1))
        , ihText = \prompt -> do
            putStrLn $ T.unpack prompt
            T.pack <$> getLine
        , ihDice = \_ _ -> error "Dice not used in delta agent"
        , ihCustom = \tag _ -> error $ "Custom request '" <> T.unpack tag <> "' not used in delta agent"
        }

      displayProposal :: Proposal -> Eff AgentEffects ()
      displayProposal proposal = do
        logInfo "Proposing:"
        forM_ proposal.propItems $ \item -> do
          let clarify = maybe "" (\c -> " (needs: " <> c <> ")") item.piNeedsClarify
          logInfo $ "  " <> item.piEntity <> " → " <> item.piDestination <> clarify

  -- Run the main loop
  -- Order must match AgentEffects type (innermost effect = innermost runner)
  -- Log comes after stubs because stub runners require Log :> es
  runEff
    . runLog config.dacLogLevel
    . runCalendarStub
    . runGitHubStub
    . runObsidianStub
    . runHabiticaStub
    . runChatHistory
    . runRequestInput inputHandler
    . fmap fst . runState emptyContext
    . runLLM config.dacLLMConfig
    $ deltaLoop deltaConfig displayProposal

-- | The main delta loop
deltaLoop
  :: DeltaConfig AgentEffects
  -> (Proposal -> Eff AgentEffects ())
  -> Eff AgentEffects ()
deltaLoop config displayProposal = do
  logInfo "Delta agent ready. Enter text to capture (Ctrl+C to exit):"
  loop
  where
    loop = do
      input <- requestText "> "
      unless (T.null input) $ do
        (results, learned) <- runDeltaWithFeedback config input displayProposal
        logInfo $ "Routed " <> T.pack (show (length results)) <> " items"
        unless (null learned) $
          logInfo $ "Learned " <> T.pack (show (length learned)) <> " new patterns"
      loop

