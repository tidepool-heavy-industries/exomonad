{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.StopHook.Graph
  ( StopHookGraph (..),
  )
where

import Control.Monad.Freer.State (State)
import ExoMonad.Control.StopHook.Types
import ExoMonad.Effects.Effector (Effector, GhPrStatusResult)
import ExoMonad.Graph.Generic (EntryNode, ExitNode, LogicNode, (:-))
import ExoMonad.Graph.Goto (Goto)
import ExoMonad.Graph.Types (Exit, Input, UsesEffects, type (:@))
import GHC.Generics (Generic)

data StopHookGraph mode = StopHookGraph
  { entry :: mode :- EntryNode AgentState,
    -- Global circuit breaker
    globalLoopCheck ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "globalMaxReached" AgentState,
                Goto "checkBuild" AgentState
              ],
    globalMaxReached ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects '[Goto Exit (TemplateName, StopHookContext)],
    -- Build stage
    checkBuild ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "routeBuild" (AgentState, BuildResult)
              ],
    routeBuild ::
      mode
        :- LogicNode
        :@ Input (AgentState, BuildResult)
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "buildContext" (AgentState, TemplateName),
                Goto "buildLoopCheck" AgentState
              ],
    buildLoopCheck ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "buildMaxReached" AgentState,
                Goto "checkTest" AgentState
              ],
    buildMaxReached ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects '[Goto "buildContext" (AgentState, TemplateName)],
    -- Test stage
    checkTest ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "routeTest" (AgentState, TestResult)
              ],
    routeTest ::
      mode
        :- LogicNode
        :@ Input (AgentState, TestResult)
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "buildContext" (AgentState, TemplateName),
                Goto "testLoopCheck" AgentState
              ],
    testLoopCheck ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "testMaxReached" AgentState,
                Goto "checkDocs" AgentState
              ],
    testMaxReached ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects '[Goto "buildContext" (AgentState, TemplateName)],
    -- Template rendering (shared by all stages)
    buildContext ::
      mode
        :- LogicNode
        :@ Input (AgentState, TemplateName)
        :@ UsesEffects
             '[ State WorkflowState,
                Goto Exit (TemplateName, StopHookContext)
              ],
    -- Docs stage
    checkDocs ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ Effector,
                State WorkflowState,
                Goto Exit (TemplateName, StopHookContext),
                Goto "checkPR" AgentState
              ],
    -- PR stage
    checkPR ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ Effector,
                State WorkflowState,
                Goto "routePR" (AgentState, GhPrStatusResult)
              ],
    routePR ::
      mode
        :- LogicNode
        :@ Input (AgentState, GhPrStatusResult)
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "buildContext" (AgentState, TemplateName),
                Goto "prLoopCheck" AgentState
              ],
    prLoopCheck ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects
             '[ State WorkflowState,
                Goto "prMaxReached" AgentState,
                Goto Exit (TemplateName, StopHookContext) -- Complete for now
              ],
    prMaxReached ::
      mode
        :- LogicNode
        :@ Input AgentState
        :@ UsesEffects '[Goto "buildContext" (AgentState, TemplateName)],
    exit :: mode :- ExitNode (TemplateName, StopHookContext)
  }
  deriving (Generic)
