{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template context types for the hybrid TDD workflow.
--
-- This module provides ToGVal instances for template rendering.
-- It is separate from Templates/Hybrid.hs for TH staging reasons.
module TypesFirstDev.Context.Hybrid
  ( -- * Re-exports from Types/Hybrid
    module TypesFirstDev.Types.Hybrid
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

import TypesFirstDev.Types.Hybrid

-- | Type alias for the ginger monad
type GingerM = Run SourcePos (Writer Text) Text

-- ════════════════════════════════════════════════════════════════════════════
-- SHARED CONTRACT TYPES
-- ════════════════════════════════════════════════════════════════════════════

instance ToGVal GingerM ConcreteExample where
  toGVal ex = dict
    [ ("description", toGVal (ceDescription ex))
    , ("input", toGVal (ceInput ex))
    , ("expected", toGVal (ceExpected ex))
    , ("edgeCase", toGVal (ceEdgeCase ex))
    ]

instance ToGVal GingerM PropertyType where
  toGVal = \case
    Inverse       -> toGVal ("Inverse" :: Text)
    Idempotent    -> toGVal ("Idempotent" :: Text)
    Commutative   -> toGVal ("Commutative" :: Text)
    Associative   -> toGVal ("Associative" :: Text)
    Identity      -> toGVal ("Identity" :: Text)
    Preservation  -> toGVal ("Preservation" :: Text)
    Boundary      -> toGVal ("Boundary" :: Text)
    Monotonic     -> toGVal ("Monotonic" :: Text)
    Other t       -> toGVal t

instance ToGVal GingerM PropertySketch where
  toGVal ps = dict
    [ ("name", toGVal ps.name)
    , ("description", toGVal ps.psDescription)
    , ("invariant", toGVal ps.invariant)
    , ("type", toGVal ps.psType)
    ]

instance ToGVal GingerM FunctionSpec where
  toGVal fn = dict
    [ ("name", toGVal fn.name)
    , ("signature", toGVal fn.signature)
    , ("brief", toGVal fn.brief)
    , ("behavior", toGVal fn.behavior)
    , ("examples", list (toGVal <$> toList fn.examples))
    , ("properties", list (toGVal <$> toList fn.properties))
    , ("priority", toGVal fn.priority)
    , ("dependsOn", list (toGVal <$> fn.dependsOn))
    ]
    where
      toList = foldr (:) []

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES AGENT OUTPUT
-- ════════════════════════════════════════════════════════════════════════════

instance ToGVal GingerM TypesAgentOutput where
  toGVal tao = dict
    [ ("typeName", toGVal tao.typeName)
    , ("typeKind", toGVal tao.typeKind)
    , ("typeDescription", toGVal tao.typeDescription)
    , ("constructors", list (toGVal <$> tao.constructors))
    , ("functions", list (toGVal <$> tao.functions))
    , ("imports", list (toGVal <$> tao.imports))
    , ("designNotes", toGVal tao.designNotes)
    , ("blocker", toGVal tao.blocker)
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- TYPE ADVERSARY TYPES
-- ════════════════════════════════════════════════════════════════════════════

instance ToGVal GingerM HoleType where
  toGVal = \case
    RepresentableInvalid -> toGVal ("RepresentableInvalid" :: Text)
    LeakyAbstraction     -> toGVal ("LeakyAbstraction" :: Text)
    PartialFunction      -> toGVal ("PartialFunction" :: Text)
    TypeConfusion        -> toGVal ("TypeConfusion" :: Text)

instance ToGVal GingerM Severity where
  toGVal = \case
    Critical      -> toGVal ("Critical" :: Text)
    Major         -> toGVal ("Major" :: Text)
    Minor         -> toGVal ("Minor" :: Text)
    Informational -> toGVal ("Informational" :: Text)

instance ToGVal GingerM TypeHole where
  toGVal th = dict
    [ ("holeType", toGVal th.holeType)
    , ("description", toGVal th.description)
    , ("invariantBroken", toGVal th.invariantBroken)
    , ("exploitSketch", toGVal th.exploitSketch)
    , ("severity", toGVal th.severity)
    , ("suggestedFix", toGVal th.suggestedFix)
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- ECHO CHANNELS & HARDENING
-- ════════════════════════════════════════════════════════════════════════════

instance ToGVal GingerM EchoChannel where
  toGVal ec = dict
    [ ("fromImpl", list (toGVal <$> ecFromImpl ec))
    , ("fromTests", list (toGVal <$> ecFromTests ec))
    ]

instance ToGVal GingerM HardeningHint where
  toGVal hh = dict
    [ ("context", toGVal (hhContext hh))
    , ("guidance", toGVal (hhGuidance hh))
    , ("source", toGVal (hhSource hh))
    ]

instance ToGVal GingerM TrivialTestsFeedback where
  toGVal ttf = dict
    [ ("whyRejected", toGVal (ttfWhyRejected ttf))
    , ("propertiesWrote", list (toGVal <$> ttfPropertiesWrote ttf))
    , ("suggestion", toGVal (ttfSuggestion ttf))
    ]

instance ToGVal GingerM ScopeLevel where
  toGVal = \case
    Leaf         -> toGVal ("Leaf" :: Text)
    Coordination -> toGVal ("Coordination" :: Text)
    System       -> toGVal ("System" :: Text)

-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE CONTEXT TYPES
-- ════════════════════════════════════════════════════════════════════════════

instance ToGVal GingerM TypesTemplateCtx where
  toGVal ctx = dict
    [ ("moduleName", toGVal ctx.moduleName)
    , ("description", toGVal ctx.description)
    , ("acceptanceCriteria", list (toGVal <$> ctx.acceptanceCriteria))
    , ("implPath", toGVal ctx.implPath)
    ]

instance ToGVal GingerM TestsTemplateCtx where
  toGVal ctx = dict
    [ ("typeName", toGVal (ttcTypeName ctx))
    , ("constructors", list (toGVal <$> ttcConstructors ctx))
    , ("functions", list (toGVal <$> ttcFunctions ctx))
    , ("testPath", toGVal (ttcTestPath ctx))
    , ("priorFeedback", toGVal (ttcPriorFeedback ctx))
    , ("echoes", toGVal (ttcEchoes ctx))
    , ("hardeningHints", list (toGVal <$> ttcHardeningHints ctx))
    ]

instance ToGVal GingerM ImplTemplateCtx where
  toGVal ctx = dict
    [ ("typeName", toGVal (itcTypeName ctx))
    , ("constructors", list (toGVal <$> itcConstructors ctx))
    , ("functions", list (toGVal <$> itcFunctions ctx))
    , ("implPath", toGVal (itcImplPath ctx))
    , ("echoes", toGVal (itcEchoes ctx))
    , ("hardeningHints", list (toGVal <$> itcHardeningHints ctx))
    ]

instance ToGVal GingerM MutationTemplateCtx where
  toGVal ctx = dict
    [ ("implPath", toGVal (mtcImplPath ctx))
    , ("testPath", toGVal (mtcTestPath ctx))
    , ("functions", list (toGVal <$> mtcFunctions ctx))
    , ("scopeLevel", toGVal (mtcScopeLevel ctx))
    ]

instance ToGVal GingerM TypeAdversaryTemplateCtx where
  toGVal ctx = dict
    [ ("types", toGVal ctx.types)
    , ("scopeLevel", toGVal ctx.scopeLevel)
    ]

instance ToGVal GingerM TypesFixTemplateCtx where
  toGVal ctx = dict
    [ ("originalTypes", toGVal ctx.originalTypes)
    , ("holes", list (toGVal <$> ctx.holes))
    , ("attempt", toGVal ctx.attempt)
    , ("priorFixes", list (toGVal <$> ctx.priorFixes))
    ]
