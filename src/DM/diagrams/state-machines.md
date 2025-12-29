# State Machines

Two nested state machines control game flow.

## GamePhase (Outer Lifecycle)

Governs session lifecycle. Most gameplay happens in `PhasePlaying`.

```mermaid
stateDiagram-v2
    [*] --> PhaseCharacterCreation

    PhaseCharacterCreation --> PhaseScenarioInit: character created
    PhaseScenarioInit --> PhasePlaying: scenario generated

    PhasePlaying --> PhaseBetweenScenes: retreat
    PhasePlaying --> PhasePlaying: pass_out (wake elsewhere)
    PhaseBetweenScenes --> PhasePlaying: BSLayLow / BSRecover / BSNewScene
    PhaseBetweenScenes --> PhaseSessionEnded: BSEndSession

    PhaseSessionEnded --> [*]

    note right of PhasePlaying
        Contains DMMood
        (inner state machine)
    end note
```

**Source:** `State.hs:313-324`

```haskell
data GamePhase
  = PhaseCharacterCreation
  | PhaseScenarioInit CharacterChoices
  | PhasePlaying ActiveScene DMMood
  | PhaseBetweenScenes BetweenScenesContext
  | PhaseSessionEnded
```

---

## DMMood (Inner Turn-by-Turn)

Active during `PhasePlaying`. Each mood has a template persona.

```mermaid
stateDiagram-v2
    [*] --> MoodScene

    MoodScene --> MoodAction: engage tool
    MoodAction --> MoodAftermath: resolve tool
    MoodAftermath --> MoodScene: accept tool

    MoodAction --> MoodBargain: dice pool empty
    MoodBargain --> MoodScene: accept_bargain (returns to previous mood)
    MoodBargain --> BetweenScenes: retreat
    MoodBargain --> MoodScene: pass_out (wake at new location)

    MoodScene --> MoodTrauma: stress = 9
    MoodAction --> MoodTrauma: stress = 9
    MoodAftermath --> MoodTrauma: stress = 9
    MoodTrauma --> MoodScene: trauma narrated

    note right of MoodScene: The Weaver
    note right of MoodAction: The Precipice
    note right of MoodAftermath: The Echo
    note right of MoodTrauma: The Crack
    note right of MoodBargain: The Hungry City
```

**Source:** `State.hs` DMMood type, `Loop.hs` transition logic

### Template Personas

| Mood | Persona | Voice |
|------|---------|-------|
| Scene | The Weaver | Shows possibilities, offers doors, never punishes directly |
| Action | The Precipice | The edge of the moment, presents dice choices |
| Aftermath | The Echo | What comes after, consequences as physics |
| Trauma | The Crack | Breaking point, assigns permanent scar |
| Bargain | The Hungry City | Offers dark deals when dice run out |
