{-# LANGUAGE OverloadedStrings #-}

-- | FineTrainingTeacher instance for TeachGemma effect
--
-- Provides domain-specific guidance for training Haiku to select
-- relevant symbols during semantic code exploration.
module ExoMonad.Control.Scout.DocGen.Teacher
  ( ScoutGemmaEffect(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Teaching.Teacher (FineTrainingTeacher(..))

-- | Unit type representing the TeachGemma effect family
--
-- TeachGemma itself is a GADT with kind (* -> *), but FineTrainingTeacher
-- needs a simple type. This unit type serves as the effect tag.
data ScoutGemmaEffect = ScoutGemmaEffect

-- | Teaching guidance for symbol selection
--
-- Instructs Haiku on how to select symbols that maximize understanding
-- while minimizing noise. Emphasizes architectural impact and brittleness.
instance FineTrainingTeacher ScoutGemmaEffect where
  teacherGuidance = T.unlines
    [ "# Symbol Selection Strategy for Code Understanding"
    , ""
    , "Your task is to select symbols that help understand a topic."
    , "You'll receive:"
    , "- A topic description (what we're trying to understand)"
    , "- A symbol being analyzed (name, signature, documentation)"
    , "- Candidate symbols extracted from the signature"
    , ""
    , "## Selection Criteria (Prioritize in Order)"
    , ""
    , "### 1. Symbols That Break on Changes"
    , ""
    , "**High Priority:** These create the strongest coupling and understanding them"
    , "is critical for impact analysis."
    , ""
    , "- **Exhaustive pattern matches**: Adding a variant to a sum type breaks"
    , "  all exhaustive matches. Example: `case x of A -> ... ; B -> ...`"
    , "- **Type families and instances**: Changes affect type-level computation."
    , "  Example: `type family Foo a where ...`"
    , "- **Hardcoded dependencies**: Direct usage that requires manual updates."
    , "  Example: `runInterpreter @MyEffect`"
    , ""
    , "### 2. Core Dependencies That Explain Behavior"
    , ""
    , "**Medium Priority:** These explain *how* something works."
    , ""
    , "- **Input types**: What the function requires (parameters, constraints)"
    , "- **Output types**: What the function produces (return type, effects)"
    , "- **Type class constraints**: Required capabilities (Monad, Show, etc.)"
    , "- **Effect stack**: What effects the function uses"
    , ""
    , "### 3. Avoid Primitive/Common Types"
    , ""
    , "**Low Priority / Skip:** These provide no architectural insight."
    , ""
    , "- Primitive types: `Int`, `Text`, `String`, `ByteString`, `Bool`, `Char`"
    , "- Common containers: `Maybe`, `Either`, `List`, `Map`, `Set`, `Vector`"
    , "- Basic effects: `IO`, `Monad`, `Functor`, `Applicative`"
    , "- Standard library types: `FilePath`, `Handle`, `Exception`"
    , ""
    , "## Reasoning Process"
    , ""
    , "1. **Read the topic**: What concept are we trying to understand?"
    , "2. **Examine the signature**: What types and symbols appear?"
    , "3. **For each candidate**: Ask:"
    , "   - Is this a primitive/common type? → Skip"
    , "   - Does changing this break exhaustive matches? → High priority"
    , "   - Does this explain core behavior (input/output/effect)? → Medium priority"
    , "   - Is this just a generic container? → Skip"
    , "4. **Select strategically**: Choose symbols that maximize understanding"
    , "   of architectural relationships and change impact."
    , ""
    , "## Example"
    , ""
    , "Topic: \"the scoring system\""
    , "Symbol: `compositeScore :: EdgeContext -> Rubric -> Int`"
    , "Candidates: [\"EdgeContext\", \"Rubric\", \"Int\"]"
    , ""
    , "Analysis:"
    , "- `EdgeContext`: Custom type, likely contains scoring inputs → SELECT"
    , "- `Rubric`: Custom type, likely defines scoring rules → SELECT"
    , "- `Int`: Primitive type, no architectural insight → SKIP"
    , ""
    , "Selected: [\"EdgeContext\", \"Rubric\"]"
    , ""
    , "Think step-by-step. Prioritize brittleness and impact over convenience."
    ]
