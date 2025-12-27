-- | DM Templates - compile-time type-checked Jinja templates
module DM.Templates
  ( -- * Templates
    dmTurnTemplate
  , compressionTemplate'

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
import DM.State (WorldState)
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
dmTurnTemplate :: Template DMContext TurnOutput DMEvent WorldState '[ThinkAsDM, SpeakAsNPC, AskPlayer, Choose, SpendDie, Engage, Resolve, Accept]
dmTurnTemplate = Template
  { templateJinja = dmTurnJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Compression template (no tools needed - uses unit state/event)
compressionTemplate' :: Template CompressionContext CompressionOutput () () '[]
compressionTemplate' = Template
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
renderCompression = render compressionTemplate'

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
-- TURN OUTPUT SCHEMA (Simplified for API grammar limits)
-- ══════════════════════════════════════════════════════════════

turnOutputJSONSchema :: JSONSchema
turnOutputJSONSchema = objectSchema
  [ ("narration", describeField "narration"
      "Narrative prose describing what happens. Use SpeakAsNPC tool for dialogue."
      (emptySchema TString))
  , ("stressDelta", describeField "stressDelta"
      "Change in stress (-9 to +9, 0 if no change)"
      (emptySchema TInteger))
  , ("coinDelta", describeField "coinDelta"
      "Change in coin (0 if no change)"
      (emptySchema TInteger))
  , ("continueScene", describeField "continueScene"
      "True to continue the scene, false to end it"
      (emptySchema TBoolean))
  ]
  ["narration", "stressDelta", "coinDelta", "continueScene"]

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

-- Scene control uses tagged union pattern since oneOf isn't supported
sceneControlSchema :: JSONSchema
sceneControlSchema = objectSchema
  [ ("controlType", describeField "controlType"
      "Type of scene control: continue, endScene, shiftLocation, or timeJump"
      (enumSchema ["continue", "endScene", "shiftLocation", "timeJump"]))
  -- Fields for endScene
  , ("endResolution", describeField "endResolution"
      "How the scene ends (required if controlType=endScene)" (emptySchema TString))
  -- Fields for shiftLocation
  , ("shiftTo", describeField "shiftTo"
      "Destination location ID (required if controlType=shiftLocation)" (emptySchema TString))
  , ("shiftTransition", describeField "shiftTransition"
      "Transition narration (required if controlType=shiftLocation)" (emptySchema TString))
  -- Fields for timeJump
  , ("jumpDuration", describeField "jumpDuration"
      "Duration description (required if controlType=timeJump)" (emptySchema TString))
  , ("jumpMontage", describeField "jumpMontage"
      "Montage narration (required if controlType=timeJump)" (emptySchema TString))
  ]
  ["controlType"]  -- Only controlType is always required

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

-- Scene beat uses tagged union pattern since oneOf isn't supported
sceneBeatSchema :: JSONSchema
sceneBeatSchema = objectSchema
  [ ("beatType", describeField "beatType"
      "Type of beat: playerAction, npcAction, environmentShift, revelation, or clockTick"
      (enumSchema ["playerAction", "npcAction", "environmentShift", "revelation", "clockTick"]))
  -- Fields for playerAction
  , ("action", describeField "action"
      "What happened (for playerAction, npcAction)" (emptySchema TString))
  , ("tags", describeField "tags"
      "Action tags (for playerAction)" (arraySchema (emptySchema TString)))
  -- Fields for npcAction
  , ("npcId", describeField "npcId"
      "Acting NPC (for npcAction)" (emptySchema TString))
  -- Fields for environmentShift
  , ("description", describeField "description"
      "Environmental change description (for environmentShift)" (emptySchema TString))
  -- Fields for revelation
  , ("secretContent", describeField "secretContent"
      "Revealed secret (for revelation)" (emptySchema TString))
  -- Fields for clockTick
  , ("clockId", describeField "clockId"
      "Ticking clock ID (for clockTick)" (emptySchema TString))
  , ("ticks", describeField "ticks"
      "Segments added (for clockTick)" (emptySchema TInteger))
  ]
  ["beatType"]  -- Only beatType is always required

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
