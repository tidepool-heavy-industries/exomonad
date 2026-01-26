{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GraphSpec (spec) where

import Test.Hspec
import Data.Aeson (ToJSON(..), FromJSON(..), Result(..), Value)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Actor.Graph
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')


-- ════════════════════════════════════════════════════════════════════════════
-- TEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Simple input message
data Message = Message { content :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Classification result
data Intent = ProcessIntent | DoneIntent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Final response
data Response = Response { reply :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = describe "Graph Execution" $ do

  describe "ExtractChoice" $ do

    it "extracts named target" $ do
      let choice :: GotoChoice '[To "process" Message]
          choice = gotoChoice @"process" (Message "hello")
          (target, _payload) = extractChoice choice
      target `shouldBe` "process"

    it "extracts exit target" $ do
      let choice :: GotoChoice '[To Exit Response]
          choice = gotoExit (Response "done")
          (target, _payload) = extractChoice choice
      target `shouldBe` "exit"

    it "extracts from multi-target choice" $ do
      let choice1 :: GotoChoice '[To "a" Message, To "b" Response, To Exit Text]
          choice1 = gotoChoice @"a" (Message "hello")
          (target1, _) = extractChoice choice1
      target1 `shouldBe` "a"

      let choice2 :: GotoChoice '[To "a" Message, To "b" Response, To Exit Text]
          choice2 = gotoChoice @"b" (Response "reply")
          (target2, _) = extractChoice choice2
      target2 `shouldBe` "b"

      let choice3 :: GotoChoice '[To "a" Message, To "b" Response, To Exit Text]
          choice3 = gotoExit @Text "final"
          (target3, _) = extractChoice choice3
      target3 `shouldBe` "exit"


  describe "wrapPureHandler" $ do

    it "wraps pure handler and routes correctly" $ do
      -- Track what gets routed
      routedRef <- newIORef Nothing

      let handler :: Message -> GotoChoice '[To Exit Response]
          handler msg = gotoExit (Response $ "processed: " <> msg.content)

          wrapped = wrapPureHandler handler
          testRouter target payload = writeIORef routedRef (Just (target, payload))

      runNodeHandler wrapped testRouter (toJSON (Message "hello"))
      result <- readIORef routedRef
      fst <$> result `shouldBe` Just "exit"


  describe "runGraphAsActors" $ do

    it "runs single-node graph" $ do
      -- Simplest graph: entry immediately exits
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", pureHandler entryHandler)
            ]

          entryHandler :: Message -> GotoChoice '[To Exit Response]
          entryHandler msg = gotoExit (Response $ "echo: " <> msg.content)

      result <- runGraphAsActors handlers (toJSON (Message "hello"))
      result `shouldBe` Response "echo: hello"

    it "runs two-node pipeline" $ do
      -- Entry -> Process -> Exit
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", pureHandler entryHandler)
            , ("process", pureHandler processHandler)
            ]

          entryHandler :: Message -> GotoChoice '[To "process" Message]
          entryHandler msg = gotoChoice @"process" msg

          processHandler :: Message -> GotoChoice '[To Exit Response]
          processHandler msg = gotoExit (Response $ "processed: " <> msg.content)

      result <- runGraphAsActors handlers (toJSON (Message "hello"))
      result `shouldBe` Response "processed: hello"

    it "runs multi-branch graph" $ do
      -- Entry -> Route (based on content) -> A or B -> Exit
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", pureHandler entryHandler)
            , ("route", pureHandler routeHandler)
            , ("branchA", pureHandler branchAHandler)
            , ("branchB", pureHandler branchBHandler)
            ]

          entryHandler :: Message -> GotoChoice '[To "route" Message]
          entryHandler msg = gotoChoice @"route" msg

          routeHandler :: Message -> GotoChoice '[To "branchA" Message, To "branchB" Message]
          routeHandler msg
            | msg.content == "A" = gotoChoice @"branchA" msg
            | otherwise = gotoChoice @"branchB" msg

          branchAHandler :: Message -> GotoChoice '[To Exit Response]
          branchAHandler _ = gotoExit (Response "took branch A")

          branchBHandler :: Message -> GotoChoice '[To Exit Response]
          branchBHandler _ = gotoExit (Response "took branch B")

      resultA <- runGraphAsActors handlers (toJSON (Message "A"))
      resultA `shouldBe` Response "took branch A"

      resultB <- runGraphAsActors handlers (toJSON (Message "B"))
      resultB `shouldBe` Response "took branch B"

    it "handles effectful handlers with per-actor memory" $ do
      -- Each actor gets its own counter that persists across calls
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", effHandlerWithMem (0 :: Int) entryHandler)
            ]

          entryHandler :: Message -> Eff '[Memory Int, IO] (GotoChoice '[To Exit Int])
          entryHandler _ = do
            updateMem @Int (+ 1)
            count <- getMem @Int
            pure $ gotoExit count

      result <- runGraphAsActors handlers (toJSON (Message "hello"))
      result `shouldBe` (1 :: Int)

    it "each actor has isolated memory" $ do
      -- Two actors, each with their own counter (different types even!)
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", effHandlerWithMem (0 :: Int) entryHandler)
            , ("process", effHandlerWithMem (0 :: Int) processHandler)
            ]

          entryHandler :: Message -> Eff '[Memory Int, IO] (GotoChoice '[To "process" Message])
          entryHandler msg = do
            updateMem @Int (+ 100)  -- Entry adds 100
            pure $ gotoChoice @"process" msg

          processHandler :: Message -> Eff '[Memory Int, IO] (GotoChoice '[To Exit Int])
          processHandler _ = do
            updateMem @Int (+ 1)  -- Process adds 1
            count <- getMem @Int  -- Should be 1, not 101
            pure $ gotoExit count

      result <- runGraphAsActors handlers (toJSON (Message "hello"))
      result `shouldBe` (1 :: Int)  -- Process has its own memory starting at 0


  describe "wrapEffHandler" $ do

    it "wraps effectful handler and routes correctly" $ do
      -- Track what gets routed
      routedRef <- newIORef Nothing

      let handler :: Message -> Eff '[IO] (GotoChoice '[To Exit Response])
          handler msg = pure $ gotoExit (Response $ "eff: " <> msg.content)

          wrapped = wrapEffHandler runM handler
          testRouter target payload = writeIORef routedRef (Just (target, payload))

      runNodeHandler wrapped testRouter (toJSON (Message "hello"))
      result <- readIORef routedRef
      fst <$> result `shouldBe` Just "exit"


  describe "Memory effect" $ do

    it "getMem reads initial state" $ do
      memRef <- newIORef (42 :: Int)
      let action :: Eff '[Memory Int, IO] Int
          action = getMem
          interpreter = runM . runMemoryWithIORef memRef

      result <- interpreter action
      result `shouldBe` 42

    it "updateMem modifies state" $ do
      memRef <- newIORef (0 :: Int)
      let action :: Eff '[Memory Int, IO] Int
          action = do
            updateMem @Int (+ 10)
            getMem
          interpreter = runM . runMemoryWithIORef memRef

      result <- interpreter action
      result `shouldBe` 10

    it "state persists across handler calls" $ do
      -- Create a counter that increments each time
      counterRef <- newIORef (0 :: Int)

      let incrementHandler :: Message -> Eff '[Memory Int, IO] (GotoChoice '[To Exit Int])
          incrementHandler _ = do
            updateMem @Int (+ 1)
            count <- getMem @Int
            pure $ gotoExit count

          interpreter :: forall a. Eff '[Memory Int, IO] a -> IO a
          interpreter = runM . runMemoryWithIORef counterRef

          wrapped = wrapEffHandler interpreter incrementHandler

      -- Track results
      resultsRef <- newIORef ([] :: [Int])
      let testRouter target payload = case target of
            "exit" -> case fromJSON payload of
              Success n -> atomicModifyIORef' resultsRef $ \rs -> (rs ++ [n], ())
              _ -> pure ()
            _ -> pure ()

      -- Call handler three times
      runNodeHandler wrapped testRouter (toJSON (Message "call1"))
      runNodeHandler wrapped testRouter (toJSON (Message "call2"))
      runNodeHandler wrapped testRouter (toJSON (Message "call3"))

      results <- readIORef resultsRef
      results `shouldBe` [1, 2, 3]


  describe "Integration: real task patterns" $ do

    it "batch processing: accumulates results across multiple items" $ do
      -- Simulate processing a batch of items, accumulating results
      -- Entry receives batch, sends items one by one to processor
      -- Processor accumulates in memory, exits when done
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", pureHandler entryHandler)
            , ("processor", effHandlerWithMem ([] :: [Text]) processorHandler)
            ]

          -- Entry unpacks batch and sends first item
          entryHandler :: [Text] -> GotoChoice '[To "processor" (Text, [Text])]
          entryHandler items = case items of
            [] -> error "Empty batch"
            (x:xs) -> gotoChoice @"processor" (x, xs)

          -- Processor: accumulate item, continue or exit
          processorHandler :: (Text, [Text]) -> Eff '[Memory [Text], IO] (GotoChoice '[To "processor" (Text, [Text]), To Exit [Text]])
          processorHandler (item, remaining) = do
            updateMem @[Text] (++ [item <> "-processed"])
            accumulated <- getMem @[Text]
            case remaining of
              [] -> pure $ gotoExit accumulated
              (next:rest) -> pure $ gotoChoice @"processor" (next, rest)

      result <- runGraphAsActors handlers (toJSON (["a", "b", "c"] :: [Text]))
      result `shouldBe` (["a-processed", "b-processed", "c-processed"] :: [Text])

    it "retry loop: retries until success with counter" $ do
      -- Simulate a task that might fail, with retry count
      -- Uses Self-like pattern via explicit routing
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", pureHandler entryHandler)
            , ("worker", effHandlerWithMem (0 :: Int) workerHandler)
            ]

          entryHandler :: Text -> GotoChoice '[To "worker" Text]
          entryHandler task = gotoChoice @"worker" task

          -- Worker: increment attempt counter, succeed on 3rd try
          workerHandler :: Text -> Eff '[Memory Int, IO] (GotoChoice '[To "worker" Text, To Exit (Text, Int)])
          workerHandler task = do
            updateMem @Int (+ 1)
            attempt <- getMem @Int
            if attempt >= 3
              then pure $ gotoExit (task <> "-done", attempt)
              else pure $ gotoChoice @"worker" task  -- retry

      result <- runGraphAsActors handlers (toJSON ("task" :: Text))
      result `shouldBe` (("task-done", 3) :: (Text, Int))

    it "stateful pipeline: each stage maintains independent state" $ do
      -- Three-stage pipeline, each stage has its own counter
      -- Proves memory isolation between actors
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", effHandlerWithMem (0 :: Int) entryHandler)
            , ("stage1", effHandlerWithMem (0 :: Int) stage1Handler)
            , ("stage2", effHandlerWithMem (0 :: Int) stage2Handler)
            ]

          entryHandler :: Int -> Eff '[Memory Int, IO] (GotoChoice '[To "stage1" Int])
          entryHandler n = do
            updateMem @Int (+ 1000)  -- entry adds 1000 to its memory
            pure $ gotoChoice @"stage1" n

          stage1Handler :: Int -> Eff '[Memory Int, IO] (GotoChoice '[To "stage2" Int])
          stage1Handler n = do
            updateMem @Int (+ 100)  -- stage1 adds 100 to its memory
            mem <- getMem @Int
            pure $ gotoChoice @"stage2" (n + mem)  -- pass input + stage1's memory

          stage2Handler :: Int -> Eff '[Memory Int, IO] (GotoChoice '[To Exit Int])
          stage2Handler n = do
            updateMem @Int (+ 10)  -- stage2 adds 10 to its memory
            mem <- getMem @Int
            pure $ gotoExit (n + mem)  -- final: input + stage2's memory

      -- Input is 5
      -- Entry adds 1000 to its own memory (not passed forward)
      -- Stage1 gets 5, adds 100 to its memory, passes 5 + 100 = 105
      -- Stage2 gets 105, adds 10 to its memory, returns 105 + 10 = 115
      result <- runGraphAsActors handlers (toJSON (5 :: Int))
      result `shouldBe` (115 :: Int)

    it "mixed memory types: each actor has different state type" $ do
      -- Prove that different actors can have different memory types
      let handlers :: Map Text HandlerBuilder
          handlers = Map.fromList
            [ ("entry", pureHandler entryHandler)  -- no memory
            , ("counter", effHandlerWithMem (0 :: Int) counterHandler)  -- Int memory
            , ("logger", effHandlerWithMem ([] :: [Text]) loggerHandler)  -- [Text] memory
            ]

          entryHandler :: Text -> GotoChoice '[To "counter" Text]
          entryHandler msg = gotoChoice @"counter" msg

          counterHandler :: Text -> Eff '[Memory Int, IO] (GotoChoice '[To "logger" (Text, Int)])
          counterHandler msg = do
            updateMem @Int (+ 1)
            count <- getMem @Int
            pure $ gotoChoice @"logger" (msg, count)

          loggerHandler :: (Text, Int) -> Eff '[Memory [Text], IO] (GotoChoice '[To Exit [Text]])
          loggerHandler (msg, count) = do
            updateMem @[Text] (++ [msg <> ":" <> T.pack (show count)])
            logs <- getMem @[Text]
            pure $ gotoExit logs

      result <- runGraphAsActors handlers (toJSON ("hello" :: Text))
      result `shouldBe` (["hello:1"] :: [Text])


-- Helper for JSON parsing in tests
fromJSON :: FromJSON a => Value -> Result a
fromJSON = Aeson.fromJSON
