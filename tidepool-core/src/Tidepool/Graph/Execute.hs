{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typed graph executor using OneOf dispatch.
--
-- This module provides fully typed graph execution without Dynamic or
-- unsafeCoerce. The key insight is that 'GotoChoice' wraps 'OneOf', where
-- position encodes which target was chosen and pattern matching gives exact
-- typed payloads.
--
-- = Design
--
-- @
-- GotoChoice '[To \"nodeA\" PayloadA, To \"nodeB\" PayloadB, To Exit Result]
--   = GotoChoice (OneOf '[PayloadA, PayloadB, Result])
--
-- Pattern matching:
--   Here payloadA      → call nodeA handler with PayloadA
--   There (Here payloadB) → call nodeB handler with PayloadB
--   There (There (Here result)) → return Result (exit)
-- @
--
-- The 'DispatchGoto' typeclass inductively pattern matches on the target list,
-- using 'HasField' to get handlers from the graph record by name.
module Tidepool.Graph.Execute
  ( -- * Dispatch Typeclass
    DispatchGoto(..)
    -- * Graph Execution
  , runGraph
  , runGraphFrom
    -- * Entry Handler Discovery
  , FindEntryHandler
    -- * Handler Invocation
  , CallHandler(..)
    -- * LLM Handler Execution
  , executeLLMHandler

    -- * Self-Loop Dispatch
  , DispatchGotoWithSelf(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Kind (Constraint, Type)
import Effectful (Effect, Eff, type (:>))
import GHC.Generics (Generic(..))
import GHC.Records (HasField(..))
import GHC.TypeLits (Symbol, KnownSymbol, TypeError, ErrorMessage(..))
import Text.Ginger.TH (TypedTemplate, runTypedTemplate)
import Text.Parsec.Pos (SourcePos)

import Tidepool.Effect (LLM, llmCall)
import Tidepool.Graph.Edges (GetNeeds)
import Tidepool.Graph.Generic (AsHandler, FieldsWithNamesOf)
import Tidepool.Graph.Generic.Core (Entry, AsGraph)
import qualified Tidepool.Graph.Generic.Core as G (Exit)
import Tidepool.Graph.Goto (GotoChoice(..), OneOf(..), To, LLMHandler(..))
import Tidepool.Graph.Template (GingerContext)
import Tidepool.Graph.Types (Exit, Self)
import Tidepool.Schema (HasJSONSchema(..), schemaToValue)


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE-LEVEL UTILITIES (local definitions)
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-level If (polykinded).
type IfMaybe :: Bool -> Maybe k -> Maybe k -> Maybe k
type family IfMaybe cond t f where
  IfMaybe 'True  t _ = t
  IfMaybe 'False _ f = f

-- | Check if an element is in a type-level list (polykinded).
type ElemType :: k -> [k] -> Bool
type family ElemType x xs where
  ElemType _ '[] = 'False
  ElemType x (x ': _) = 'True
  ElemType x (_ ': rest) = ElemType x rest


-- ════════════════════════════════════════════════════════════════════════════
-- ENTRY HANDLER DISCOVERY
-- ════════════════════════════════════════════════════════════════════════════

-- | Find the first field whose node definition accepts the entry type.
--
-- Iterates through (fieldName, nodeDef) pairs from 'FieldsWithNamesOf' and
-- returns the first field name where 'GetNeeds' contains the entry type.
--
-- @
-- -- For a graph with:
-- --   entry   :: mode :- Entry Int
-- --   compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[...]
-- --   exit    :: mode :- Exit Int
-- --
-- FindEntryHandler Int fields = 'Just "compute"
-- @
type FindEntryHandler :: Type -> [(Symbol, Type)] -> Maybe Symbol
type family FindEntryHandler entryType fields where
  FindEntryHandler _ '[] = 'Nothing
  FindEntryHandler entryType ('(name, Entry _) ': rest) =
    FindEntryHandler entryType rest  -- Skip Entry marker
  FindEntryHandler entryType ('(name, G.Exit _) ': rest) =
    FindEntryHandler entryType rest  -- Skip Exit marker
  FindEntryHandler entryType ('(name, def) ': rest) =
    IfMaybe (ElemType entryType (GetNeeds def))
            ('Just name)
            (FindEntryHandler entryType rest)


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a graph starting from a named handler.
--
-- Use this when you want to explicitly specify which handler to start from,
-- bypassing automatic entry handler discovery.
--
-- @
-- result <- runGraphFrom @"compute" handlers inputValue
-- @
runGraphFrom
  :: forall (name :: Symbol) graph entryType targets exitType es handler.
     ( KnownSymbol name
     , HasField name (graph (AsHandler es)) handler
     , CallHandler handler entryType es targets
     , DispatchGoto graph targets es exitType
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraphFrom graph input = do
  let handler = getField @name graph
  choice <- callHandler handler input
  dispatchGoto graph choice


-- | Run a graph from Entry to Exit.
--
-- Automatically discovers the first handler that accepts the entry type
-- (via the 'Needs' annotation), calls it with the input, and dispatches
-- through the graph until Exit is reached.
--
-- @
-- -- Define graph
-- data TestGraph mode = TestGraph
--   { entry   :: mode :- Entry Int
--   , compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Exit Int]
--   , exit    :: mode :- Exit Int
--   }
--
-- -- Run it
-- result <- runGraph handlers 5  -- Returns 6 (if compute does +1)
-- @
runGraph
  :: forall graph entryType targets exitType es entryHandlerName handler.
     ( Generic (graph AsGraph)
     , FindEntryHandler entryType (FieldsWithNamesOf graph) ~ 'Just entryHandlerName
     , KnownSymbol entryHandlerName
     , HasField entryHandlerName (graph (AsHandler es)) handler
     , CallHandler handler entryType es targets
     , DispatchGoto graph targets es exitType
     )
  => graph (AsHandler es)
  -> entryType
  -> Eff es exitType
runGraph = runGraphFrom @entryHandlerName


-- ════════════════════════════════════════════════════════════════════════════
-- LLM HANDLER EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute an LLMBoth handler, returning a GotoChoice.
--
-- This function:
-- 1. Calls the before-handler to build template context
-- 2. Renders the system template (if provided) and user template
-- 3. Calls the LLM effect with rendered prompts and JSON schema
-- 4. Parses the structured output
-- 5. Calls the after-handler to determine the next transition
--
-- = Type Parameters
--
-- * @needs@ - The input type from Needs annotation
-- * @schema@ - The LLM output schema type
-- * @targets@ - The transition targets from UsesEffects
-- * @es@ - The effect stack (must include LLM)
-- * @tpl@ - The template context type
--
-- = Example
--
-- @
-- result <- executeLLMHandler
--   Nothing                          -- no system template
--   userTemplate                     -- user prompt template
--   (\\input -> pure MyContext {...}) -- build context
--   (\\output -> pure (gotoExit output)) -- route based on output
--   inputValue
-- @
executeLLMHandler
  :: forall needs schema targets es tpl.
     ( LLM :> es
     , FromJSON schema
     , HasJSONSchema schema
     , GingerContext tpl
     )
  => Maybe (TypedTemplate tpl SourcePos)      -- ^ Optional system prompt template
  -> TypedTemplate tpl SourcePos              -- ^ User prompt template (required)
  -> (needs -> Eff es tpl)                    -- ^ Before handler: builds context
  -> (schema -> Eff es (GotoChoice targets))  -- ^ After handler: routes based on output
  -> needs                                    -- ^ Input from Needs
  -> Eff es (GotoChoice targets)
executeLLMHandler mSystemTpl userTpl beforeFn afterFn input = do
  -- Build context from before-handler
  ctx <- beforeFn input
  -- Render templates
  let systemPrompt = maybe "" (runTypedTemplate ctx) mSystemTpl
      userPrompt = runTypedTemplate ctx userTpl
      schemaVal = schemaToValue (jsonSchema @schema)
  -- Call LLM with rendered prompts
  output <- llmCall @schema systemPrompt userPrompt schemaVal
  -- Route based on output
  afterFn output


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER INVOCATION TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for invoking handlers uniformly.
--
-- This abstracts over the difference between:
-- * Logic node handlers: @payload -> Eff es (GotoChoice targets)@
-- * LLM node handlers: @LLMHandler payload schema targets es tpl@
--
-- By using this typeclass, the main dispatch instance doesn't need to know
-- which kind of handler it's dealing with - it just calls 'callHandler'.
type CallHandler :: Type -> Type -> [Effect] -> [Type] -> Constraint
class CallHandler handler payload es targets | handler -> payload es targets where
  callHandler :: handler -> payload -> Eff es (GotoChoice targets)

-- | Logic node handler: simple function invocation.
instance CallHandler (payload -> Eff es (GotoChoice targets)) payload es targets where
  callHandler = id

-- ════════════════════════════════════════════════════════════════════════════
-- COMPILE-TIME VALIDATION FOR LLM HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Validate that LLM handler has non-empty targets.
--
-- LLMBefore forces @targets = '[]@, which makes graph dispatch impossible
-- because there are no Goto targets to route to. This constraint catches
-- LLMBefore at compile time.
--
-- Note: LLMBoth cannot have @targets = '[]@ in practice because you cannot
-- construct a value of type @GotoChoice '[]@ (it contains @OneOf '[]@ which
-- is uninhabited). So this constraint precisely identifies LLMBefore.
type ValidLLMTargets :: [Type] -> Constraint
type family ValidLLMTargets targets where
  ValidLLMTargets '[] = TypeError
    ( 'Text "LLMBefore is not supported for graph dispatch."
      ':$$: 'Text ""
      ':$$: 'Text "LLMBefore has targets = '[] (no explicit Goto targets),"
      ':$$: 'Text "but graph dispatch requires routing information to"
      ':$$: 'Text "determine which node to execute next."
      ':$$: 'Text ""
      ':$$: 'Text "Fix: Use LLMBoth instead, which provides both template"
      ':$$: 'Text "context (before) and explicit routing (after)."
    )
  ValidLLMTargets _ = ()

-- | Validate that LLM handler has non-unit template context.
--
-- LLMAfter forces @tpl = ()@, which means no template context is available.
-- Graph dispatch needs to render LLM prompts using typed template contexts.
-- This constraint catches LLMAfter at compile time.
--
-- Note: If you genuinely need an empty context with LLMBoth, define a custom
-- unit type: @data NoContext = NoContext@ and use that instead of @()@.
type ValidLLMContext :: Type -> Constraint
type family ValidLLMContext tpl where
  ValidLLMContext () = TypeError
    ( 'Text "LLMAfter is not supported for graph dispatch."
      ':$$: 'Text ""
      ':$$: 'Text "LLMAfter has tpl = () (no template context),"
      ':$$: 'Text "but graph dispatch needs to render LLM prompts"
      ':$$: 'Text "using typed template contexts."
      ':$$: 'Text ""
      ':$$: 'Text "Fix: Use LLMBoth instead, which provides both template"
      ':$$: 'Text "context (before) and explicit routing (after)."
      ':$$: 'Text ""
      ':$$: 'Text "If you genuinely need an empty context, define a custom"
      ':$$: 'Text "unit type: 'data NoContext = NoContext'"
    )
  ValidLLMContext _ = ()

-- | LLM node handler: execute via executeLLMHandler.
--
-- Only 'LLMBoth' is supported for graph execution because it provides both:
--
-- 1. A template context builder (before phase) for rendering prompts
-- 2. Explicit Goto targets (after phase) for typed dispatch
--
-- 'LLMBefore' and 'LLMAfter' are caught at __compile time__ via type family
-- constraints that detect their characteristic type signatures:
--
-- * 'LLMBefore' forces @targets = '[]@ → caught by 'ValidLLMTargets'
-- * 'LLMAfter' forces @tpl = ()@ → caught by 'ValidLLMContext'
instance
  ( LLM :> es
  , FromJSON schema
  , HasJSONSchema schema
  , GingerContext tpl
  , ValidLLMTargets targets  -- Compile-time: catches LLMBefore
  , ValidLLMContext tpl      -- Compile-time: catches LLMAfter
  ) => CallHandler (LLMHandler payload schema targets es tpl) payload es targets where
  callHandler (LLMBoth mSysTpl userTpl beforeFn afterFn) p =
    executeLLMHandler mSysTpl userTpl beforeFn afterFn p
  -- The following patterns are unreachable due to the ValidLLMTargets and
  -- ValidLLMContext constraints above. GHC knows that:
  --   - LLMBefore would require targets ~ '[], contradicting ValidLLMTargets
  --   - LLMAfter would require tpl ~ (), contradicting ValidLLMContext
  -- We keep them for pattern exhaustiveness with explicit unreachable markers.
  callHandler (LLMBefore _) _ =
    error "unreachable: LLMBefore is caught at compile time by ValidLLMTargets"
  callHandler (LLMAfter _) _ =
    error "unreachable: LLMAfter is caught at compile time by ValidLLMContext"


-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch on a 'GotoChoice', recursively executing handlers until Exit.
--
-- This typeclass enables fully typed dispatch through a graph. Each instance
-- handles one case of the target list, recursively calling handlers until
-- an Exit target is reached.
--
-- = Type Parameters
--
-- * @graph@ - The graph record type (e.g., @TestGraph@)
-- * @targets@ - The current target list to dispatch on
-- * @es@ - The effect stack available to handlers
-- * @exitType@ - The graph's exit type (what 'runGraph' returns)
--
-- = How It Works
--
-- Given @GotoChoice '[To \"a\" A, To \"b\" B, To Exit R]@:
--
-- 1. Pattern match on @OneOf '[A, B, R]@
-- 2. If @Here payload@: call handler \"a\" with payload :: A
-- 3. If @There (Here payload)@: call handler \"b\" with payload :: B
-- 4. If @There (There (Here result))@: return result :: R
--
-- Each handler returns its own @GotoChoice@, so dispatch recurses through
-- the graph until Exit is reached.
--
-- = Example
--
-- @
-- -- Given a graph with compute handler that exits
-- result <- dispatchGoto testHandlers (gotoChoice \@"compute" 5)
-- -- Internally: calls compute handler, gets GotoChoice '[To Exit Int]
-- -- Pattern matches Here 6, returns 6
-- @
type DispatchGoto :: (Type -> Type) -> [Type] -> [Effect] -> Type -> Constraint
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR CASE: EMPTY TARGET LIST
-- ════════════════════════════════════════════════════════════════════════════

-- | Error case: Empty target list is invalid.
--
-- A GotoChoice must have at least one target. This instance produces a clear
-- type error rather than an opaque "No instance" message.
instance TypeError
  ('Text "Cannot dispatch on empty target list"
   ':$$: 'Text ""
   ':$$: 'Text "A GotoChoice must have at least one target (typically To Exit exitType)."
   ':$$: 'Text "Check that your UsesEffects annotation includes valid Goto effects."
  ) => DispatchGoto graph '[] es exitType where
  dispatchGoto = error "unreachable: empty target list"


-- ════════════════════════════════════════════════════════════════════════════
-- EXIT INSTANCES
-- ════════════════════════════════════════════════════════════════════════════

-- | Base case: Exit is the only target.
--
-- When there's only @To Exit exitType@ in the target list, pattern matching
-- gives us the exit value directly.
instance {-# OVERLAPPING #-} DispatchGoto graph '[To Exit exitType] es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result

-- | Exit is first, but there are more targets.
--
-- Handle the Exit case (return result) or skip to rest of targets.
instance {-# OVERLAPPABLE #-}
  ( DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To Exit exitType ': rest) es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result
  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- SELF-LOOP INSTANCES (error guidance)
-- ════════════════════════════════════════════════════════════════════════════

-- | Self-loop as only target: use DispatchGotoWithSelf instead.
--
-- Self-loops require tracking the "current" handler to re-invoke.
-- This instance provides a clear error directing users to the correct API.
instance TypeError
  ('Text "Self-loop dispatch requires DispatchGotoWithSelf"
   ':$$: 'Text ""
   ':$$: 'Text "To Self transitions need an explicit self-handler."
   ':$$: 'Text "Use dispatchGotoWithSelf instead of dispatchGoto."
  ) => DispatchGoto graph '[To Self payload] es exitType where
  dispatchGoto = error "unreachable: self-loop"

-- | Self first with more targets: use DispatchGotoWithSelf instead.
instance {-# OVERLAPPABLE #-} TypeError
  ('Text "Self-loop dispatch requires DispatchGotoWithSelf"
   ':$$: 'Text ""
   ':$$: 'Text "To Self transitions need an explicit self-handler."
   ':$$: 'Text "Use dispatchGotoWithSelf instead of dispatchGoto."
  ) => DispatchGoto graph (To Self payload ': rest) es exitType where
  dispatchGoto = error "unreachable: self-loop"


-- ════════════════════════════════════════════════════════════════════════════
-- NAMED NODE INSTANCE
-- ════════════════════════════════════════════════════════════════════════════

-- | Named node target: call the handler via 'CallHandler' and recurse.
--
-- When the first target is @To (name :: Symbol) payload@:
--
-- 1. Use 'HasField' to get the handler from the graph record
-- 2. Use 'CallHandler' to invoke the handler (works for both Logic and LLM nodes)
-- 3. Recursively dispatch on the handler's returned 'GotoChoice'
--
-- The @handler@ type is inferred from the graph record, and 'CallHandler'
-- determines how to invoke it based on whether it's a function or LLMHandler.
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) handler
  , CallHandler handler payload es handlerTargets
  , DispatchGoto graph handlerTargets es exitType
  , DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To (name :: Symbol) payload ': rest) es exitType where

  dispatchGoto graph (GotoChoice (Here payload)) = do
    let handler = getField @name graph
    nextChoice <- callHandler handler payload
    dispatchGoto graph nextChoice

  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)


-- ════════════════════════════════════════════════════════════════════════════
-- SELF-LOOP DISPATCH
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch with an explicit self-handler for nodes that use 'Goto Self'.
--
-- When a node can transition back to itself via 'Goto Self', the standard
-- 'dispatchGoto' doesn't know which handler to re-invoke. This typeclass
-- solves this by taking the self-handler as an explicit parameter.
--
-- The key difference from 'DispatchGoto' is that the self-handler is threaded
-- through all recursive calls, so when 'Goto Self' is encountered, the handler
-- can be invoked with the new payload.
--
-- = Usage
--
-- For nodes with self-loops:
--
-- @
-- -- Define handler that may loop back to itself
-- loopHandler :: Int -> Eff es (GotoChoice '[To Self Int, To Exit Int])
-- loopHandler n
--   | n >= 10   = pure $ gotoExit n
--   | otherwise = pure $ gotoSelf (n + 1)
--
-- -- Run with explicit self-handler
-- initialChoice <- loopHandler 0
-- result <- dispatchGotoWithSelf loopHandler graph initialChoice
-- @
--
-- Note: The @allTargets@ parameter is the full target list that the self-handler
-- returns. This ensures type consistency when recursing back via Self.
type DispatchGotoWithSelf :: (Type -> Type) -> Type -> [Type] -> [Type] -> [Effect] -> Type -> Constraint
class DispatchGotoWithSelf graph selfPayload allTargets targets es exitType where
  dispatchGotoWithSelf
    :: (selfPayload -> Eff es (GotoChoice allTargets))  -- ^ Self-handler (returns full target list)
    -> graph (AsHandler es)                              -- ^ Graph handlers
    -> GotoChoice targets                                -- ^ Current choice (may be subset)
    -> Eff es exitType

-- | Base case: Exit is the only target.
instance DispatchGotoWithSelf graph selfPayload allTargets '[To Exit exitType] es exitType where
  dispatchGotoWithSelf _ _ (GotoChoice (Here result)) = pure result

-- | Self is first target: call self-handler and recurse with full target list.
instance
  ( DispatchGotoWithSelf graph selfPayload allTargets allTargets es exitType
  , DispatchGotoWithSelf graph selfPayload allTargets rest es exitType
  ) => DispatchGotoWithSelf graph selfPayload allTargets (To Self selfPayload ': rest) es exitType where
  dispatchGotoWithSelf selfHandler graph (GotoChoice (Here payload)) = do
    -- Call self-handler, which returns GotoChoice allTargets
    nextChoice <- selfHandler payload
    -- Recurse with full target list
    dispatchGotoWithSelf @graph @selfPayload @allTargets @allTargets selfHandler graph nextChoice
  dispatchGotoWithSelf selfHandler graph (GotoChoice (There rest)) =
    dispatchGotoWithSelf @graph @selfPayload @allTargets @rest selfHandler graph (GotoChoice rest)

-- | Exit in current position: handle exit or skip.
instance
  ( DispatchGotoWithSelf graph selfPayload allTargets rest es exitType
  ) => DispatchGotoWithSelf graph selfPayload allTargets (To Exit exitType ': rest) es exitType where
  dispatchGotoWithSelf _ _ (GotoChoice (Here result)) = pure result
  dispatchGotoWithSelf selfHandler graph (GotoChoice (There rest)) =
    dispatchGotoWithSelf @graph @selfPayload @allTargets @rest selfHandler graph (GotoChoice rest)

-- | Named node target: call handler and recurse.
--
-- Note: When a named handler returns a GotoChoice, we dispatch on it with the
-- handler's target list, not allTargets. The self-handler is still threaded
-- through for nested Self transitions.
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) (payload -> Eff es (GotoChoice handlerTargets))
  , DispatchGotoWithSelf graph selfPayload allTargets handlerTargets es exitType
  , DispatchGotoWithSelf graph selfPayload allTargets rest es exitType
  ) => DispatchGotoWithSelf graph selfPayload allTargets (To (name :: Symbol) payload ': rest) es exitType where
  dispatchGotoWithSelf selfHandler graph (GotoChoice (Here payload)) = do
    let handler = getField @name graph
    nextChoice <- handler payload
    dispatchGotoWithSelf @graph @selfPayload @allTargets @handlerTargets selfHandler graph nextChoice
  dispatchGotoWithSelf selfHandler graph (GotoChoice (There rest)) =
    dispatchGotoWithSelf @graph @selfPayload @allTargets @rest selfHandler graph (GotoChoice rest)
