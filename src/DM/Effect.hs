{-# LANGUAGE TypeFamilies #-}
-- | DM-specific effects for phase-aware state access
--
-- PlayingState provides type-safe access to scene and mood,
-- requiring callers to pattern match on PhasePlaying to provide
-- the initial values.
module DM.Effect
  ( -- * PlayingState Effect
    PlayingState(..)
  , getScene
  , getMood
  , putScene
  , putMood
  , modifyMood
  , modifyScene
    -- * Interpreter
  , runPlayingState
  ) where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, send, LocalEnv)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import DM.State (ActiveScene, DMMood)

-- ══════════════════════════════════════════════════════════════
-- PLAYING STATE EFFECT
-- ══════════════════════════════════════════════════════════════

-- | Effect for accessing scene and mood during PhasePlaying
--
-- This effect is only provided when the game loop has pattern-matched
-- on PhasePlaying to extract the scene and mood. Tools that require
-- this effect document their phase requirement in their type signature.
data PlayingState :: Effect where
  GetScene :: PlayingState m ActiveScene
  GetMood :: PlayingState m DMMood
  PutScene :: ActiveScene -> PlayingState m ()
  PutMood :: DMMood -> PlayingState m ()

type instance DispatchOf PlayingState = 'Dynamic

-- | Get the current scene
getScene :: PlayingState :> es => Eff es ActiveScene
getScene = send GetScene

-- | Get the current mood
getMood :: PlayingState :> es => Eff es DMMood
getMood = send GetMood

-- | Set the current scene
putScene :: PlayingState :> es => ActiveScene -> Eff es ()
putScene = send . PutScene

-- | Set the current mood
putMood :: PlayingState :> es => DMMood -> Eff es ()
putMood = send . PutMood

-- | Modify the current mood
modifyMood :: PlayingState :> es => (DMMood -> DMMood) -> Eff es ()
modifyMood f = getMood >>= putMood . f

-- | Modify the current scene
modifyScene :: PlayingState :> es => (ActiveScene -> ActiveScene) -> Eff es ()
modifyScene f = getScene >>= putScene . f

-- ══════════════════════════════════════════════════════════════
-- INTERPRETER
-- ══════════════════════════════════════════════════════════════

-- | Run PlayingState effect with initial scene and mood
--
-- The caller must pattern match on PhasePlaying to provide these values,
-- ensuring this effect is only available during active play.
-- Returns the result along with the final scene and mood.
runPlayingState
  :: IOE :> es
  => ActiveScene
  -> DMMood
  -> Eff (PlayingState : es) a
  -> Eff es (a, ActiveScene, DMMood)
runPlayingState initialScene initialMood action = do
  -- Interpret PlayingState using IORef for scene and mood
  -- This is simpler than nested State effects
  sceneRef <- liftIO $ newIORef initialScene
  moodRef <- liftIO $ newIORef initialMood
  result <- interpret (handler sceneRef moodRef) action
  finalScene <- liftIO $ readIORef sceneRef
  finalMood <- liftIO $ readIORef moodRef
  return (result, finalScene, finalMood)
  where
    handler
      :: IOE :> es'
      => IORef ActiveScene
      -> IORef DMMood
      -> LocalEnv localEs es'
      -> PlayingState (Eff localEs) a'
      -> Eff es' a'
    handler sceneRef moodRef _env = \case
      GetScene -> liftIO $ readIORef sceneRef
      GetMood -> liftIO $ readIORef moodRef
      PutScene s -> liftIO $ writeIORef sceneRef s
      PutMood m -> liftIO $ writeIORef moodRef m
