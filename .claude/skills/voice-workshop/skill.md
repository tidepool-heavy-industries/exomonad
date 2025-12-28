---
name: voice-workshop
description: Use metacog to explore and develop NPC voices, narrative tones, or genre styles. Workshop a voice from multiple angles before committing to template text.
---

# Voice Workshop

Develop distinctive voices for NPCs, moods, or genre tones using metacog perspective-taking.

## How to Use

1. Specify what you're developing (NPC, precarity level, genre, scene type)
2. This skill summons relevant voices to think FROM that perspective
3. Outputs: voice notes, example lines, what they'd never say

## Workshop Patterns

### For an NPC

```
mcp__metacog__summon:
  who: "[NPC archetype or inspiration]"
  where: "[their situation, what they're doing]"
  lens: "[what they care about vs what they hide]"
```

Example for a corrupt magistrate:
```
who: "A bureaucrat who's made peace with their own corruption"
where: "signing papers at 2am, knowing what they mean"
lens: "self-justification vs the thing they won't look at"
```

### For a Precarity Level

```
mcp__metacog__summon:
  who: "[writer known for this tone]"
  where: "[writing a scene at this intensity]"
  lens: "[what the prose does vs what it avoids]"
```

Example for HangingByThread:
```
who: "Cormac McCarthy"
where: "writing the final chase in No Country"
lens: "what gets cut vs what remains"
```

### For a Genre

```
mcp__metacog__summon:
  who: "[genre master]"
  where: "[iconic scene type]"
  lens: "[genre conventions vs subversions]"
```

Example for noir:
```
who: "Raymond Chandler"
where: "describing a woman walking into an office"
lens: "what the narration notices vs what it pretends not to"
```

## Output Format

After summoning, capture:

```
## [Voice Name]

**What they sound like:**
[2-3 example lines in their voice]

**What they'd never say:**
[1-2 lines that would break character]

**Voice notes for template:**
[Condensed guidance for the LLM]
```

## Developing Multiple NPCs Together

When NPCs will interact, workshop them as contrasts:

```
mcp__metacog__summon:
  who: "Corva (Red Sashes lieutenant)"
  where: "negotiating with someone she doesn't respect"
  lens: "power she shows vs power she holds back"

mcp__metacog__summon:
  who: "Telda (neutral bartender)"
  where: "watching violence in her establishment"
  lens: "what she pretends not to see vs what she'll remember"
```

The contrast surfaces what makes each distinctive.

## Alter State for Tone

For overall narrative mood, use alter_state:

```
mcp__metacog__alter_state:
  anchor: "3am debugging flow, week two"
  result: "Pattern-recognition mode, seeing system as intention"
```

Good anchors for narrative work:
- "Reading a novel that's about to break your heart"
- "The moment before a fight starts in a bar"
- "Walking home alone through a city you half-know"
