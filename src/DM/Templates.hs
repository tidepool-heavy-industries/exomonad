-- | DM Templates - compile-time type-checked Jinja templates
module DM.Templates
  ( -- * Templates
    dmTurnTemplate
  , compressionTemplate

    -- * Typed Jinja Templates
  , dmTurnJinja
  , compressionJinja

    -- * Rendering
  , renderDMTurn
  , renderCompression

    -- * Schemas
  , turnOutputSchema
  , compressionOutputSchema
  ) where

import DM.Context
import DM.Output
import DM.Tools
import Tidepool.Template
import Tidepool.Schema
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)

-- ══════════════════════════════════════════════════════════════
-- TYPED JINJA TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | DM turn Jinja template (compile-time validated)
dmTurnJinja :: TypedTemplate DMContext SourcePos
dmTurnJinja = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")

-- | Compression Jinja template (compile-time validated)
compressionJinja :: TypedTemplate CompressionContext SourcePos
compressionJinja = $(typedTemplateFile ''CompressionContext "templates/compression.jinja")

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | Main DM turn template with all available tools
dmTurnTemplate :: Template DMContext TurnOutput DMEvent '[ThinkAsDM, SpeakAsNPC, AskPlayer, Choose]
dmTurnTemplate = Template
  { templateJinja = dmTurnJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = TCons (Proxy @ThinkAsDM)
                  $ TCons (Proxy @SpeakAsNPC)
                  $ TCons (Proxy @AskPlayer)
                  $ TCons (Proxy @Choose)
                  $ TNil
  }

-- | Compression template (no tools needed - uses unit event type)
compressionTemplate :: Template CompressionContext CompressionOutput () '[]
compressionTemplate = Template
  { templateJinja = compressionJinja
  , templateOutputSchema = compressionOutputSchema
  , templateTools = TNil
  }

-- ══════════════════════════════════════════════════════════════
-- RENDERING
-- ══════════════════════════════════════════════════════════════

-- | Render DM turn template with context
renderDMTurn :: DMContext -> Text
renderDMTurn = render dmTurnTemplate

-- | Render compression template with context
renderCompression :: CompressionContext -> Text
renderCompression = render compressionTemplate

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS
-- ══════════════════════════════════════════════════════════════

-- | Schema for the main turn output structure
turnOutputSchema :: Schema TurnOutput
turnOutputSchema = Schema
  { schemaJSON = schemaToValue turnOutputJSONSchema
  , schemaDescription = "DM turn output with narration and world state mutations"
  }

-- | Schema for scene compression output
compressionOutputSchema :: Schema CompressionOutput
compressionOutputSchema = Schema
  { schemaJSON = schemaToValue compressionOutputJSONSchema
  , schemaDescription = "Scene compression output for persisting durable changes"
  }

-- ══════════════════════════════════════════════════════════════
-- TURN OUTPUT SCHEMA
-- ══════════════════════════════════════════════════════════════

turnOutputJSONSchema :: JSONSchema
turnOutputJSONSchema = objectSchema
  [ ("narration", describeField "narration"
      "The narrative prose for this turn. Describe what happens in response to player action."
      (emptySchema TString))
  , ("playerDeltas", describeField "playerDeltas"
      "Changes to player resources (stress, coin, heat, wanted)"
      playerDeltasSchema)
  , ("newTrauma", describeField "newTrauma"
      "New trauma if stress exceeded 9 (resets stress to 0)"
      (emptySchema TString))
  , ("requestOutcomes", describeField "requestOutcomes"
      "Request player to choose a die for a risky action"
      requestOutcomesSchema)
  , ("clearPendingOutcome", describeField "clearPendingOutcome"
      "Set true after narrating a locked dice outcome"
      (emptySchema TBoolean))
  , ("clockTicks", describeField "clockTicks"
      "Advance existing clocks"
      (arraySchema clockTickSchema))
  , ("newClocks", describeField "newClocks"
      "Create new countdown clocks"
      (arraySchema newClockSchema))
  , ("revealClocks", describeField "revealClocks"
      "Clock IDs to reveal to the player"
      (arraySchema (emptySchema TString)))
  , ("newThreads", describeField "newThreads"
      "Create new narrative threads"
      (arraySchema newThreadSchema))
  , ("threadEscalations", describeField "threadEscalations"
      "Escalate thread tension levels"
      (arraySchema threadEscalationSchema))
  , ("resolvedThreads", describeField "resolvedThreads"
      "Mark threads as resolved"
      (arraySchema threadResolutionSchema))
  , ("attitudeShifts", describeField "attitudeShifts"
      "Shift faction attitudes toward/away from player"
      (arraySchema attitudeShiftSchema))
  , ("factionLearns", describeField "factionLearns"
      "Facts that factions learn"
      (arraySchema factionLearnsSchema))
  , ("dispositionShifts", describeField "dispositionShifts"
      "Shift NPC dispositions toward/away from player"
      (arraySchema dispositionShiftSchema))
  , ("npcMoves", describeField "npcMoves"
      "Move NPCs to new locations"
      (arraySchema npcMoveSchema))
  , ("npcReveals", describeField "npcReveals"
      "NPCs reveal secrets to the player"
      (arraySchema npcRevealSchema))
  , ("sceneControl", describeField "sceneControl"
      "Scene flow control (continue, end scene, shift location, time jump)"
      sceneControlSchema)
  , ("spreadRumors", describeField "spreadRumors"
      "New rumors spreading in the world"
      (arraySchema newRumorSchema))
  , ("confirmRumors", describeField "confirmRumors"
      "Confirm rumor truth values"
      (arraySchema confirmRumorSchema))
  ]
  ["narration", "playerDeltas", "clearPendingOutcome", "sceneControl"]

-- ══════════════════════════════════════════════════════════════
-- HELPER SCHEMAS FOR TURN OUTPUT
-- ══════════════════════════════════════════════════════════════

playerDeltasSchema :: JSONSchema
playerDeltasSchema = objectSchema
  [ ("stressDelta", describeField "stressDelta" "Change in stress (-9 to +9)" (emptySchema TInteger))
  , ("coinDelta", describeField "coinDelta" "Change in coin" (emptySchema TInteger))
  , ("heatDelta", describeField "heatDelta" "Change in heat (0-10)" (emptySchema TInteger))
  , ("wantedDelta", describeField "wantedDelta" "Change in wanted level (0-4)" (emptySchema TInteger))
  , ("deltaBecause", describeField "deltaBecause" "Why these changes happened" (emptySchema TString))
  ]
  ["stressDelta", "coinDelta", "heatDelta", "wantedDelta", "deltaBecause"]

requestOutcomesSchema :: JSONSchema
requestOutcomesSchema = objectSchema
  [ ("requestContext", describeField "requestContext"
      "What the player is trying to do" (emptySchema TString))
  , ("requestPosition", describeField "requestPosition"
      "Risk level of the action" positionSchema)
  , ("requestEffect", describeField "requestEffect"
      "Potential effect magnitude" effectSchema)
  , ("requestStakes", describeField "requestStakes"
      "What happens on failure" (emptySchema TString))
  ]
  ["requestContext", "requestPosition", "requestEffect", "requestStakes"]

positionSchema :: JSONSchema
positionSchema = enumSchema ["Controlled", "Risky", "Desperate"]

effectSchema :: JSONSchema
effectSchema = enumSchema ["Limited", "Standard", "Great"]

tensionSchema :: JSONSchema
tensionSchema = enumSchema ["Simmering", "Rising", "Urgent", "Boiling"]

directionSchema :: JSONSchema
directionSchema = enumSchema ["Toward", "Away"]

degreeSchema :: JSONSchema
degreeSchema = enumSchema ["Slight", "Notable", "Major"]

clockTickSchema :: JSONSchema
clockTickSchema = objectSchema
  [ ("tickClock", describeField "tickClock" "ID of clock to advance" (emptySchema TString))
  , ("tickSegments", describeField "tickSegments" "Segments to add (usually 1)" (emptySchema TInteger))
  , ("tickBecause", describeField "tickBecause" "Why the clock advances" (emptySchema TString))
  ]
  ["tickClock", "tickSegments", "tickBecause"]

newClockSchema :: JSONSchema
newClockSchema = objectSchema
  [ ("newClockName", describeField "newClockName" "Display name for the clock" (emptySchema TString))
  , ("newClockSegments", describeField "newClockSegments" "Total segments (4, 6, or 8)" (emptySchema TInteger))
  , ("newClockVisible", describeField "newClockVisible" "Whether player can see this clock" (emptySchema TBoolean))
  , ("newClockConsequence", describeField "newClockConsequence" "What happens when filled" (emptySchema TString))
  , ("newClockBecause", describeField "newClockBecause" "Why this clock exists" (emptySchema TString))
  ]
  ["newClockName", "newClockSegments", "newClockVisible", "newClockConsequence", "newClockBecause"]

newThreadSchema :: JSONSchema
newThreadSchema = objectSchema
  [ ("newThreadHook", describeField "newThreadHook" "The narrative hook/description" (emptySchema TString))
  , ("newThreadTension", describeField "newThreadTension" "Starting tension level" tensionSchema)
  , ("newThreadInvolves", describeField "newThreadInvolves" "NPC IDs involved" (arraySchema (emptySchema TString)))
  , ("newThreadBecause", describeField "newThreadBecause" "Why this thread emerges" (emptySchema TString))
  ]
  ["newThreadHook", "newThreadTension", "newThreadInvolves", "newThreadBecause"]

threadEscalationSchema :: JSONSchema
threadEscalationSchema = objectSchema
  [ ("threadId", describeField "threadId" "ID of thread to escalate" (emptySchema TString))
  , ("newTension", describeField "newTension" "New tension level" tensionSchema)
  , ("because", describeField "because" "Why tension changes" (emptySchema TString))
  ]
  ["threadId", "newTension", "because"]

threadResolutionSchema :: JSONSchema
threadResolutionSchema = objectSchema
  [ ("threadId", describeField "threadId" "ID of thread to resolve" (emptySchema TString))
  , ("resolutionOutcome", describeField "resolutionOutcome" "How the thread resolved" (emptySchema TString))
  , ("resolutionBecause", describeField "resolutionBecause" "Why it resolved this way" (emptySchema TString))
  ]
  ["threadId", "resolutionOutcome", "resolutionBecause"]

attitudeShiftSchema :: JSONSchema
attitudeShiftSchema = objectSchema
  [ ("shiftFaction", describeField "shiftFaction" "Faction ID" (emptySchema TString))
  , ("shiftDirection", describeField "shiftDirection" "Toward (friendlier) or Away (hostile)" directionSchema)
  , ("shiftDegree", describeField "shiftDegree" "Magnitude of shift" degreeSchema)
  , ("shiftBecause", describeField "shiftBecause" "Why attitude shifts" (emptySchema TString))
  ]
  ["shiftFaction", "shiftDirection", "shiftDegree", "shiftBecause"]

factionLearnsSchema :: JSONSchema
factionLearnsSchema = objectSchema
  [ ("factionId", describeField "factionId" "Faction learning the fact" (emptySchema TString))
  , ("factContent", describeField "factContent" "The fact content" (emptySchema TString))
  , ("factSource", describeField "factSource" "How they learned it" (emptySchema TString))
  , ("because", describeField "because" "Why they learn this now" (emptySchema TString))
  ]
  ["factionId", "factContent", "factSource", "because"]

dispositionShiftSchema :: JSONSchema
dispositionShiftSchema = objectSchema
  [ ("dispShiftNpc", describeField "dispShiftNpc" "NPC ID" (emptySchema TString))
  , ("dispShiftDirection", describeField "dispShiftDirection" "Toward (friendlier) or Away" directionSchema)
  , ("dispShiftDegree", describeField "dispShiftDegree" "Magnitude of shift" degreeSchema)
  , ("dispShiftBecause", describeField "dispShiftBecause" "Why disposition changes" (emptySchema TString))
  ]
  ["dispShiftNpc", "dispShiftDirection", "dispShiftDegree", "dispShiftBecause"]

npcMoveSchema :: JSONSchema
npcMoveSchema = objectSchema
  [ ("npcId", describeField "npcId" "NPC to move" (emptySchema TString))
  , ("locationId", describeField "locationId" "Destination location" (emptySchema TString))
  , ("because", describeField "because" "Why they move" (emptySchema TString))
  ]
  ["npcId", "locationId", "because"]

npcRevealSchema :: JSONSchema
npcRevealSchema = objectSchema
  [ ("revealNpc", describeField "revealNpc" "NPC revealing the secret" (emptySchema TString))
  , ("revealSecret", describeField "revealSecret" "Content of the secret" (emptySchema TString))
  , ("revealBecause", describeField "revealBecause" "Why they reveal this now" (emptySchema TString))
  ]
  ["revealNpc", "revealSecret", "revealBecause"]

sceneControlSchema :: JSONSchema
sceneControlSchema = oneOfSchema
  [ objectSchema [("continue", emptySchema TBoolean)] ["continue"]
  , objectSchema
      [ ("endScene", emptySchema TBoolean)
      , ("endResolution", describeField "endResolution" "How the scene ends" (emptySchema TString))
      ]
      ["endScene", "endResolution"]
  , objectSchema
      [ ("shiftLocation", emptySchema TBoolean)
      , ("shiftTo", describeField "shiftTo" "Destination location ID" (emptySchema TString))
      , ("shiftTransition", describeField "shiftTransition" "Transition narration" (emptySchema TString))
      ]
      ["shiftLocation", "shiftTo", "shiftTransition"]
  , objectSchema
      [ ("timeJump", emptySchema TBoolean)
      , ("jumpDuration", describeField "jumpDuration" "Duration description" (emptySchema TString))
      , ("jumpMontage", describeField "jumpMontage" "Montage narration" (emptySchema TString))
      ]
      ["timeJump", "jumpDuration", "jumpMontage"]
  ]

truthValueSchema :: JSONSchema
truthValueSchema = enumSchema ["TrueRumor", "FalseRumor", "PartiallyTrue"]

spreadLevelSchema :: JSONSchema
spreadLevelSchema = enumSchema ["Whisper", "Tavern", "CommonKnowledge"]

newRumorSchema :: JSONSchema
newRumorSchema = objectSchema
  [ ("newRumorContent", describeField "newRumorContent" "The rumor content" (emptySchema TString))
  , ("newRumorTruth", describeField "newRumorTruth" "Truth value of the rumor" truthValueSchema)
  , ("newRumorSpread", describeField "newRumorSpread" "How widely spread" spreadLevelSchema)
  , ("newRumorBecause", describeField "newRumorBecause" "Why this rumor emerges" (emptySchema TString))
  ]
  ["newRumorContent", "newRumorTruth", "newRumorSpread", "newRumorBecause"]

confirmRumorSchema :: JSONSchema
confirmRumorSchema = objectSchema
  [ ("rumorId", describeField "rumorId" "ID of rumor to confirm" (emptySchema TString))
  , ("because", describeField "because" "How it was confirmed" (emptySchema TString))
  ]
  ["rumorId", "because"]

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION OUTPUT SCHEMA
-- ══════════════════════════════════════════════════════════════

compressionOutputJSONSchema :: JSONSchema
compressionOutputJSONSchema = objectSchema
  [ ("sceneOutcome", describeField "sceneOutcome"
      "Summary of what happened in the scene"
      sceneOutcomeSchema)
  , ("worldDeltas", describeField "worldDeltas"
      "Changes to world state from the scene"
      worldDeltasSchema)
  , ("extracted", describeField "extracted"
      "New narrative elements extracted from the scene"
      extractionsSchema)
  , ("decay", describeField "decay"
      "Elements that decay or fade over time"
      decaySchema)
  ]
  ["sceneOutcome", "worldDeltas", "extracted", "decay"]

sceneOutcomeSchema :: JSONSchema
sceneOutcomeSchema = objectSchema
  [ ("outcomeSummary", describeField "outcomeSummary"
      "One paragraph summary of what happened" (emptySchema TString))
  , ("outcomeKeyBeats", describeField "outcomeKeyBeats"
      "The most important moments to remember" (arraySchema sceneBeatSchema))
  , ("outcomePlayerChoices", describeField "outcomePlayerChoices"
      "Significant choices the player made" (arraySchema playerChoiceSchema))
  , ("outcomeConsequenceSeeds", describeField "outcomeConsequenceSeeds"
      "Seeds for future consequences" (arraySchema (emptySchema TString)))
  ]
  ["outcomeSummary", "outcomeKeyBeats", "outcomePlayerChoices", "outcomeConsequenceSeeds"]

sceneBeatSchema :: JSONSchema
sceneBeatSchema = oneOfSchema
  [ objectSchema
      [ ("playerAction", emptySchema TBoolean)
      , ("action", describeField "action" "What the player did" (emptySchema TString))
      , ("tags", describeField "tags" "Action tags" (arraySchema (emptySchema TString)))
      ]
      ["playerAction", "action"]
  , objectSchema
      [ ("npcAction", emptySchema TBoolean)
      , ("npcId", describeField "npcId" "Acting NPC" (emptySchema TString))
      , ("action", describeField "action" "What the NPC did" (emptySchema TString))
      ]
      ["npcAction", "npcId", "action"]
  , objectSchema
      [ ("environmentShift", emptySchema TBoolean)
      , ("description", describeField "description" "Environmental change" (emptySchema TString))
      ]
      ["environmentShift", "description"]
  , objectSchema
      [ ("revelation", emptySchema TBoolean)
      , ("secretContent", describeField "secretContent" "Revealed secret" (emptySchema TString))
      ]
      ["revelation", "secretContent"]
  , objectSchema
      [ ("clockTick", emptySchema TBoolean)
      , ("clockId", describeField "clockId" "Ticking clock" (emptySchema TString))
      , ("ticks", describeField "ticks" "Segments added" (emptySchema TInteger))
      ]
      ["clockTick", "clockId", "ticks"]
  ]

choiceWeightSchema :: JSONSchema
choiceWeightSchema = enumSchema ["Trivial", "Meaningful", "Pivotal"]

playerChoiceSchema :: JSONSchema
playerChoiceSchema = objectSchema
  [ ("choiceDescription", describeField "choiceDescription" "What the player chose" (emptySchema TString))
  , ("choiceAlternatives", describeField "choiceAlternatives" "What they could have chosen" (arraySchema (emptySchema TString)))
  , ("choiceWeight", describeField "choiceWeight" "How significant was this choice" choiceWeightSchema)
  , ("choiceBecause", describeField "choiceBecause" "Why this choice matters" (emptySchema TString))
  ]
  ["choiceDescription", "choiceAlternatives", "choiceWeight", "choiceBecause"]

attitudeSchema :: JSONSchema
attitudeSchema = enumSchema ["Hostile", "Wary", "Neutral", "Favorable", "Allied"]

goalStatusSchema :: JSONSchema
goalStatusSchema = enumSchema ["Pursuing", "Blocked", "Achieved", "Abandoned"]

worldDeltasSchema :: JSONSchema
worldDeltasSchema = objectSchema
  [ ("factionDeltas", describeField "factionDeltas"
      "Changes to faction states" (arraySchema factionDeltaSchema))
  , ("npcMemories", describeField "npcMemories"
      "Memories NPCs form from the scene" (arraySchema npcMemorySchema))
  , ("locationChanges", describeField "locationChanges"
      "Location description updates" (arraySchema locationChangeSchema))
  ]
  ["factionDeltas", "npcMemories", "locationChanges"]

factionDeltaSchema :: JSONSchema
factionDeltaSchema = objectSchema
  [ ("deltaFaction", describeField "deltaFaction" "Faction ID" (emptySchema TString))
  , ("deltaAttitudeChange", describeField "deltaAttitudeChange"
      "Attitude before/after/reason (optional)" attitudeChangeSchema)
  , ("deltaGoalProgress", describeField "deltaGoalProgress"
      "Goal status updates" (arraySchema goalProgressSchema))
  , ("deltaNewSecrets", describeField "deltaNewSecrets"
      "New secrets the faction learns" (arraySchema (emptySchema TString)))
  ]
  ["deltaFaction"]

attitudeChangeSchema :: JSONSchema
attitudeChangeSchema = objectSchema
  [ ("from", describeField "from" "Previous attitude" attitudeSchema)
  , ("to", describeField "to" "New attitude" attitudeSchema)
  , ("reason", describeField "reason" "Why attitude changed" (emptySchema TString))
  ]
  ["from", "to", "reason"]

goalProgressSchema :: JSONSchema
goalProgressSchema = objectSchema
  [ ("goalId", describeField "goalId" "Goal ID" (emptySchema TString))
  , ("newStatus", describeField "newStatus" "New goal status" goalStatusSchema)
  ]
  ["goalId", "newStatus"]

npcMemorySchema :: JSONSchema
npcMemorySchema = objectSchema
  [ ("npcId", describeField "npcId" "NPC forming the memory" (emptySchema TString))
  , ("memory", describeField "memory" "What they remember" (emptySchema TString))
  ]
  ["npcId", "memory"]

locationChangeSchema :: JSONSchema
locationChangeSchema = objectSchema
  [ ("locationId", describeField "locationId" "Location to update" (emptySchema TString))
  , ("newDescription", describeField "newDescription" "Updated description" (emptySchema TString))
  ]
  ["locationId", "newDescription"]

extractionsSchema :: JSONSchema
extractionsSchema = objectSchema
  [ ("extractedThreads", describeField "extractedThreads"
      "New narrative threads from the scene" (arraySchema newThreadSchema))
  , ("extractedRumors", describeField "extractedRumors"
      "New rumors from the scene" (arraySchema newRumorSchema))
  , ("extractedPromises", describeField "extractedPromises"
      "Promises made during the scene" (arraySchema npcTextPairSchema))
  , ("extractedDebts", describeField "extractedDebts"
      "Debts incurred during the scene" (arraySchema npcTextPairSchema))
  , ("extractedInsults", describeField "extractedInsults"
      "Insults to factions during the scene" (arraySchema factionTextPairSchema))
  , ("extractedFavors", describeField "extractedFavors"
      "Favors earned during the scene" (arraySchema npcTextPairSchema))
  ]
  ["extractedThreads", "extractedRumors", "extractedPromises", "extractedDebts", "extractedInsults", "extractedFavors"]

npcTextPairSchema :: JSONSchema
npcTextPairSchema = objectSchema
  [ ("npcId", describeField "npcId" "NPC involved" (emptySchema TString))
  , ("content", describeField "content" "Description" (emptySchema TString))
  ]
  ["npcId", "content"]

factionTextPairSchema :: JSONSchema
factionTextPairSchema = objectSchema
  [ ("factionId", describeField "factionId" "Faction involved" (emptySchema TString))
  , ("content", describeField "content" "Description" (emptySchema TString))
  ]
  ["factionId", "content"]

decaySchema :: JSONSchema
decaySchema = objectSchema
  [ ("decayRumors", describeField "decayRumors"
      "Rumor IDs that fade away" (arraySchema (emptySchema TString)))
  , ("decayThreads", describeField "decayThreads"
      "Thread IDs that resolve quietly" (arraySchema (emptySchema TString)))
  , ("decayTensionReductions", describeField "decayTensionReductions"
      "Threads with reduced tension" (arraySchema tensionReductionSchema))
  , ("decayClocks", describeField "decayClocks"
      "Clocks that tick backwards (fade)" (arraySchema clockDecaySchema))
  ]
  ["decayRumors", "decayThreads", "decayTensionReductions", "decayClocks"]

tensionReductionSchema :: JSONSchema
tensionReductionSchema = objectSchema
  [ ("threadId", describeField "threadId" "Thread ID" (emptySchema TString))
  , ("newTension", describeField "newTension" "Reduced tension level" tensionSchema)
  ]
  ["threadId", "newTension"]

clockDecaySchema :: JSONSchema
clockDecaySchema = objectSchema
  [ ("clockId", describeField "clockId" "Clock ID" (emptySchema TString))
  , ("segmentsLost", describeField "segmentsLost" "Segments to remove" (emptySchema TInteger))
  ]
  ["clockId", "segmentsLost"]
