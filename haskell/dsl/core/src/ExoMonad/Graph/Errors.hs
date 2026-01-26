
-- | Educational type errors for the Graph DSL.
--
-- = Philosophy
--
-- Every type error is an opportunity to teach the developer:
--
-- 1. **What they tried to do** (reflect their intent)
-- 2. **Why it failed** (the invariant they violated)
-- 3. **How the system works** (the underlying model)
-- 4. **How to fix it** (concrete steps)
--
-- = Structure
--
-- Errors follow a consistent visual format:
--
-- @
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ❌ Error Title
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- WHAT HAPPENED:
--   Your code...
--
-- HOW IT WORKS:
--   Explanation of the invariant...
--
-- FIXES:
--   1. Do this
--   2. Or this
-- @
module ExoMonad.Graph.Errors
  ( -- * Error Formatting Primitives
    HR
  , Section
  , CodeLine
  , Bullet
  , Indent
  , Blank

    -- * Section Headers
  , WhatHappened
  , HowItWorks
  , Fixes
  , Example

    -- * Symbol Utilities
  , FormatSymbolList
  , FormatTargetList
  , ExtractTargetNames

    -- * Pre-defined Error Messages
  , SelfLoopDispatchError

    -- * Type Error Infrastructure (from GHC.TypeError)
    -- | Re-exported from "GHC.TypeError" for use in validation modules.
    --
    -- == Semantic Distinction
    --
    -- * 'TypeError' - "You're missing something" (user can add annotation, fix typo)
    -- * 'Unsatisfiable' - "These constraints are contradictory" (fundamental redesign needed)
    -- * 'Assert' - Conditional check that emits error on failure
  , Unsatisfiable
  , unsatisfiable
  , Assert
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol, ErrorMessage(..))
import GHC.TypeError (Unsatisfiable, unsatisfiable, Assert)

-- ════════════════════════════════════════════════════════════════════════════
-- FORMATTING PRIMITIVES
-- ════════════════════════════════════════════════════════════════════════════

-- | Horizontal rule for visual separation.
type HR :: ErrorMessage
type HR = 'Text "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

-- | A section with a header.
type Section :: Symbol -> ErrorMessage -> ErrorMessage
type Section header content =
  'Text "" ':$$: 'Text header ':$$: content

-- | A line of code (indented).
type CodeLine :: Symbol -> ErrorMessage
type CodeLine code = 'Text "     " ':<>: 'Text code

-- | A bullet point.
type Bullet :: Symbol -> ErrorMessage
type Bullet item = 'Text "   • " ':<>: 'Text item

-- | Indented text.
type Indent :: Symbol -> ErrorMessage
type Indent text = 'Text "   " ':<>: 'Text text

-- | Blank line.
type Blank :: ErrorMessage
type Blank = 'Text ""

-- ════════════════════════════════════════════════════════════════════════════
-- SECTION HEADERS
-- ════════════════════════════════════════════════════════════════════════════

-- | "WHAT HAPPENED" section header.
type WhatHappened :: ErrorMessage
type WhatHappened = 'Text "WHAT HAPPENED:"

-- | "HOW IT WORKS" section header.
type HowItWorks :: ErrorMessage
type HowItWorks = 'Text "HOW IT WORKS:"

-- | "FIXES" section header.
type Fixes :: ErrorMessage
type Fixes = 'Text "FIXES:"

-- | "EXAMPLE" section header.
type Example :: ErrorMessage
type Example = 'Text "EXAMPLE:"

-- ════════════════════════════════════════════════════════════════════════════
-- SYMBOL LIST FORMATTING
-- ════════════════════════════════════════════════════════════════════════════

-- | Format a list of symbols as bullet points.
type FormatSymbolList :: [Symbol] -> ErrorMessage
type family FormatSymbolList ss where
  FormatSymbolList '[] = 'Text "   (none)"
  FormatSymbolList '[s] = Bullet s
  FormatSymbolList (s ': rest) = Bullet s ':$$: FormatSymbolList rest

-- | Extract target names from To markers.
--
-- @
-- ExtractTargetNames '[To "a" X, To "b" Y, To Exit Z] = '["a", "b"]
-- @
type ExtractTargetNames :: [Type] -> [Symbol]
type family ExtractTargetNames targets where
  ExtractTargetNames '[] = '[]
  ExtractTargetNames (To (name :: Symbol) _ ': rest) = name ': ExtractTargetNames rest
  ExtractTargetNames (To Exit _ ': rest) = ExtractTargetNames rest  -- Skip Exit
  ExtractTargetNames (To Self _ ': rest) = ExtractTargetNames rest  -- Skip Self
  ExtractTargetNames (_ ': rest) = ExtractTargetNames rest

-- | Format a target list showing both names and payload types.
type FormatTargetList :: [Type] -> ErrorMessage
type family FormatTargetList targets where
  FormatTargetList '[] = 'Text "   (none)"
  FormatTargetList '[t] = FormatTarget t
  FormatTargetList (t ': rest) = FormatTarget t ':$$: FormatTargetList rest

type FormatTarget :: Type -> ErrorMessage
type family FormatTarget t where
  FormatTarget (To (name :: Symbol) payload) =
    'Text "   • \"" ':<>: 'Text name ':<>: 'Text "\" with payload " ':<>: 'ShowType payload
  FormatTarget (To Exit payload) =
    'Text "   • Exit with payload " ':<>: 'ShowType payload
  FormatTarget (To Self payload) =
    'Text "   • Self with payload " ':<>: 'ShowType payload
  FormatTarget other =
    'Text "   • " ':<>: 'ShowType other

-- | Placeholder type for 'To' from Goto.hs
--
-- This is a duplicate definition that exists here for error message formatting.
-- The canonical definition is in ExoMonad.Graph.Goto. We cannot import it here
-- due to module dependency ordering (Errors.hs is imported by Goto.hs).
type To :: k -> Type -> Type
data To target payload

-- | Placeholder type for 'Exit' from Types.hs
--
-- Duplicate for error formatting. Canonical definition in ExoMonad.Graph.Types.
type Exit :: Type
data Exit

-- | Placeholder type for 'Self' from Goto.hs
--
-- Duplicate for error formatting. Canonical definition in ExoMonad.Graph.Goto.
type Self :: Type
data Self

-- | Error message for self-loop dispatch requiring DispatchGotoWithSelf
--
-- This error is shown when a handler uses 'gotoSelf' but the caller uses
-- 'dispatchGoto' instead of 'dispatchGotoWithSelf'.
type SelfLoopDispatchError =
  HR
  ':$$: 'Text "  Self-loop requires special dispatch"
  ':$$: HR
  ':$$: Blank
  ':$$: WhatHappened
  ':$$: Indent "Your handler can 'gotoSelf', but you called 'dispatchGoto'."
  ':$$: Indent "The standard dispatcher doesn't know which handler to re-invoke."
  ':$$: Blank
  ':$$: HowItWorks
  ':$$: Indent "Normal dispatch:  GotoChoice -> find handler by name -> call it"
  ':$$: Indent "Self dispatch:    GotoChoice -> ??? -> call... which handler?"
  ':$$: Blank
  ':$$: Indent "The graph record has fields like 'compute', 'route', etc."
  ':$$: Indent "But there's no 'self' field! We need you to tell us what"
  ':$$: Indent "'self' means for this particular dispatch."
  ':$$: Blank
  ':$$: Fixes
  ':$$: Bullet "Use dispatchGotoWithSelf and pass the self-handler:"
  ':$$: Blank
  ':$$: CodeLine "-- Instead of:"
  ':$$: CodeLine "choice <- loopHandler input"
  ':$$: CodeLine "result <- dispatchGoto handlers choice        -- ERROR"
  ':$$: Blank
  ':$$: CodeLine "-- Use:"
  ':$$: CodeLine "choice <- loopHandler input"
  ':$$: CodeLine "result <- dispatchGotoWithSelf loopHandler handlers choice  -- OK"
  ':$$: CodeLine "                               ^^^^^^^^^^^"
  ':$$: CodeLine "                               \"when you see Self, call this\""
