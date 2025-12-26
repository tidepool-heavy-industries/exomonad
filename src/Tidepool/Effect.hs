{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Core effect types for Tidepool game loops
module Tidepool.Effect
  ( -- * Core Effects
    GameEffects
  , State(..)
  , Random(..)
  , LLM(..)
  , Emit(..)
  , ControlFlow(..)
    
    -- * Effect Operations
  , get
  , gets
  , put
  , modify
  , random
  , weightedChoice
  , llm
  , emit
  , yield
  , continue
  
    -- * Running Effects
  , runGame
  , runState
  , runRandom
  , runLLM
  , runEmit
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The effect stack for game loops
type GameEffects s event =
  '[ State s
   , Random
   , LLM
   , Emit event
   ]

-- ══════════════════════════════════════════════════════════════
-- STATE EFFECT
-- ══════════════════════════════════════════════════════════════

data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()

type instance DispatchOf (State s) = 'Dynamic

get :: State s :> es => Eff es s
get = send Get

gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: State s :> es => s -> Eff es ()
put = send . Put

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = get >>= put . f

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState _initial = error "TODO: runState - reinterpret using Effectful.State.Static.Local"

-- ══════════════════════════════════════════════════════════════
-- RANDOM EFFECT
-- ══════════════════════════════════════════════════════════════

data Random :: Effect where
  RandomDouble :: Random m Double
  WeightedChoice :: [(Double, a)] -> Random m a

type instance DispatchOf Random = 'Dynamic

random :: Random :> es => Eff es Double
random = send RandomDouble

weightedChoice :: Random :> es => [(Double, a)] -> Eff es a
weightedChoice opts = send (WeightedChoice opts)

runRandom :: Eff (Random : es) a -> Eff es a
runRandom = error "TODO: runRandom - interpret using System.Random.Stateful"

-- ══════════════════════════════════════════════════════════════
-- LLM EFFECT
-- ══════════════════════════════════════════════════════════════

data LLM :: Effect where
  CallLLM :: template -> input -> LLM m output

type instance DispatchOf LLM = 'Dynamic

llm :: LLM :> es => template -> input -> Eff es output
llm template input = send (CallLLM template input)

data LLMConfig = LLMConfig
  { apiKey :: Text
  , model :: Text
  , maxTokens :: Int
  }
  deriving (Show, Eq, Generic)

runLLM :: LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLM _config = error "TODO: runLLM - call Anthropic API with template rendering and structured output parsing"

-- ══════════════════════════════════════════════════════════════
-- EMIT EFFECT
-- ══════════════════════════════════════════════════════════════

data Emit event :: Effect where
  EmitEvent :: event -> Emit event m ()

type instance DispatchOf (Emit event) = 'Dynamic

emit :: Emit event :> es => event -> Eff es ()
emit = send . EmitEvent

runEmit :: (event -> IO ()) -> Eff (Emit event : es) a -> Eff es a
runEmit _handler = error "TODO: runEmit - interpret by calling handler in IO"

-- ══════════════════════════════════════════════════════════════
-- CONTROL FLOW EFFECT
-- ══════════════════════════════════════════════════════════════

data ControlFlow response :: Effect where
  Yield :: response -> ControlFlow response m a
  Continue :: ControlFlow response m ()

type instance DispatchOf (ControlFlow r) = 'Dynamic

yield :: ControlFlow r :> es => r -> Eff es a
yield = send . Yield

continue :: ControlFlow r :> es => Eff es ()
continue = send Continue

-- ══════════════════════════════════════════════════════════════
-- COMBINED RUNNER
-- ══════════════════════════════════════════════════════════════

runGame 
  :: s
  -> LLMConfig
  -> (event -> IO ())
  -> Eff (GameEffects s event) a 
  -> IO (a, s)
runGame _initialState _llmConfig _eventHandler _computation = 
  error "TODO: runGame - compose runState, runRandom, runLLM, runEmit via runEff"
