-- | Tarot cards for story seeding
--
-- Full 78-card deck: Major Arcana + Minor Arcana (Wands, Cups, Swords, Pentacles)
-- Cards serve as random seeds for scenario generation, providing thematic
-- flavor and narrative hooks for the LLM.
module DM.Tarot
  ( TarotCard(..)
  , Suit(..)
  , Rank(..)
  , allCards
  , drawSpread
  , cardName
  , cardMeaning
  , spreadDescription
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Text.Ginger.GVal (ToGVal(..))
import Text.Ginger.GVal.Generic (genericToGVal)

-- | Suits of the Minor Arcana
data Suit = Wands | Cups | Swords | Pentacles
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Ranks within a suit
data Rank
  = Ace | Two | Three | Four | Five | Six | Seven
  | Eight | Nine | Ten | Page | Knight | Queen | King
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | A tarot card - either Major or Minor Arcana
data TarotCard
  -- Major Arcana
  = TheFool
  | TheMagician
  | TheHighPriestess
  | TheEmpress
  | TheEmperor
  | TheHierophant
  | TheLovers
  | TheChariot
  | Strength
  | TheHermit
  | WheelOfFortune
  | Justice
  | TheHangedMan
  | Death
  | Temperance
  | TheDevil
  | TheTower
  | TheStar
  | TheMoon
  | TheSun
  | Judgement
  | TheWorld
  -- Minor Arcana
  | MinorCard Suit Rank
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | All 78 cards in the deck
allCards :: [TarotCard]
allCards = majorArcana ++ minorArcana
  where
    majorArcana =
      [ TheFool, TheMagician, TheHighPriestess, TheEmpress, TheEmperor
      , TheHierophant, TheLovers, TheChariot, Strength, TheHermit
      , WheelOfFortune, Justice, TheHangedMan, Death, Temperance
      , TheDevil, TheTower, TheStar, TheMoon, TheSun, Judgement, TheWorld
      ]
    minorArcana =
      [ MinorCard suit rank
      | suit <- [Wands, Cups, Swords, Pentacles]
      , rank <- [Ace .. King]
      ]

-- | Draw a spread of N unique cards
drawSpread :: Int -> IO [TarotCard]
drawSpread n = go n allCards []
  where
    go 0 _ acc = pure (reverse acc)
    go _ [] acc = pure (reverse acc)
    go remaining deck acc = do
      idx <- randomRIO (0, length deck - 1)
      let card = deck !! idx
          deck' = take idx deck ++ drop (idx + 1) deck
      go (remaining - 1) deck' (card : acc)

-- | Get the display name of a card
cardName :: TarotCard -> Text
cardName card = case card of
  TheFool -> "The Fool"
  TheMagician -> "The Magician"
  TheHighPriestess -> "The High Priestess"
  TheEmpress -> "The Empress"
  TheEmperor -> "The Emperor"
  TheHierophant -> "The Hierophant"
  TheLovers -> "The Lovers"
  TheChariot -> "The Chariot"
  Strength -> "Strength"
  TheHermit -> "The Hermit"
  WheelOfFortune -> "Wheel of Fortune"
  Justice -> "Justice"
  TheHangedMan -> "The Hanged Man"
  Death -> "Death"
  Temperance -> "Temperance"
  TheDevil -> "The Devil"
  TheTower -> "The Tower"
  TheStar -> "The Star"
  TheMoon -> "The Moon"
  TheSun -> "The Sun"
  Judgement -> "Judgement"
  TheWorld -> "The World"
  MinorCard suit rank -> rankName rank <> " of " <> suitName suit

rankName :: Rank -> Text
rankName = \case
  Ace -> "Ace"
  Two -> "Two"
  Three -> "Three"
  Four -> "Four"
  Five -> "Five"
  Six -> "Six"
  Seven -> "Seven"
  Eight -> "Eight"
  Nine -> "Nine"
  Ten -> "Ten"
  Page -> "Page"
  Knight -> "Knight"
  Queen -> "Queen"
  King -> "King"

suitName :: Suit -> Text
suitName = \case
  Wands -> "Wands"
  Cups -> "Cups"
  Swords -> "Swords"
  Pentacles -> "Pentacles"

-- | Get the thematic meaning of a card (Doskvol-flavored)
cardMeaning :: TarotCard -> Text
cardMeaning card = case card of
  -- Major Arcana
  TheFool -> "New beginnings, innocence, a leap into the unknown. A fresh face in the underworld, unburdened by debts—yet."
  TheMagician -> "Skill, resourcefulness, power through will. A whisper who can make things happen, connections in strange places."
  TheHighPriestess -> "Intuition, secrets, the unconscious. The spirit wardens know more than they tell, and so do you."
  TheEmpress -> "Abundance, nurturing, protection. Someone powerful watches over you—the Dimmer Sisters, perhaps, or a noble patron."
  TheEmperor -> "Authority, structure, control. The Bluecoats, the nobility, the weight of law demanding submission."
  TheHierophant -> "Tradition, institutions, old debts. The Church of Ecstasy, powers that predate the Emperor."
  TheLovers -> "Partnership, choices, alignment. An alliance that will define you, a choice between loyalties."
  TheChariot -> "Willpower, determination, victory through control. A score requiring precision, enemies to outmaneuver."
  Strength -> "Courage, patience, inner power. The demons you've mastered, the hull you've learned to wear."
  TheHermit -> "Introspection, solitude, guidance. A life in shadows, a mentor's ghost, secrets learned alone."
  WheelOfFortune -> "Cycles, fate, turning points. The gangs rise and fall; today's victim is tomorrow's boss."
  Justice -> "Fairness, truth, cause and effect. Debts paid in coin or blood. The Crows remember everything."
  TheHangedMan -> "Surrender, sacrifice, new perspectives. Sometimes you lose to win. A sacrifice that opens doors."
  Death -> "Endings, transformation, transition. The ghost you carry, the life left behind, what made you."
  Temperance -> "Balance, patience, the long game. The careful work of a long con, poison taking its time."
  TheDevil -> "Bondage, addiction, dark bargains. The vice that owns you, leviathan blood, the deal you regret."
  TheTower -> "Upheaval, revelation, sudden change. A score gone wrong, betrayal, the moment everything burns."
  TheStar -> "Hope, renewal, inspiration. A glimpse of something better, the reason you keep going."
  TheMoon -> "Illusion, fear, the subconscious. Ghosts that haunt you, deathseeker cults, things that shouldn't exist."
  TheSun -> "Success, vitality, joy. Brightstone's lights, wealth that blinds, a moment of genuine triumph."
  Judgement -> "Reflection, reckoning, awakening. The past catches up, spirit bells toll, redemption or damnation."
  TheWorld -> "Completion, accomplishment, wholeness. A score that changes everything, an ending and a beginning."

  -- Minor Arcana - Wands (Fire: passion, creativity, action, conflict)
  MinorCard Wands Ace -> "A spark of inspiration, new venture, raw potential. The first match struck in darkness."
  MinorCard Wands Two -> "Planning, decisions, looking ahead. Two paths through the Dusk, both dangerous."
  MinorCard Wands Three -> "Expansion, foresight, enterprise. Your crew's reputation spreads beyond Crow's Foot."
  MinorCard Wands Four -> "Celebration, harmony, homecoming. A brief moment of peace in the eternal night."
  MinorCard Wands Five -> "Competition, conflict, rivalry. Other crews want what you have. Blades will cross."
  MinorCard Wands Six -> "Victory, recognition, advancement. A score well done, respect earned in blood."
  MinorCard Wands Seven -> "Perseverance, defensive position, standing ground. Enemies at the gate, holding the line."
  MinorCard Wands Eight -> "Speed, urgency, rapid change. News travels fast in Doskvol. Move faster."
  MinorCard Wands Nine -> "Resilience, persistence, last stand. Wounded but fighting, one more score to survive."
  MinorCard Wands Ten -> "Burden, responsibility, hard work. The weight of the crew on your shoulders."
  MinorCard Wands Page -> "Enthusiasm, exploration, discovery. A young contact with dangerous information."
  MinorCard Wands Knight -> "Energy, passion, impulsiveness. A hotheaded ally—or enemy—forcing the pace."
  MinorCard Wands Queen -> "Courage, determination, independence. A powerful woman who answers to no one."
  MinorCard Wands King -> "Leadership, vision, entrepreneurship. A boss with ambition and the will to seize it."

  -- Minor Arcana - Cups (Water: emotions, relationships, intuition)
  MinorCard Cups Ace -> "New feelings, intuition, emotional beginning. A connection that changes everything."
  MinorCard Cups Two -> "Partnership, attraction, unified love. Trust in Doskvol—rarer than radiant energy."
  MinorCard Cups Three -> "Friendship, community, celebration. Your crew, your family, worth dying for."
  MinorCard Cups Four -> "Apathy, contemplation, disconnection. The numbness that comes from too much loss."
  MinorCard Cups Five -> "Loss, grief, disappointment. Someone you trusted is gone. The betrayal stings."
  MinorCard Cups Six -> "Nostalgia, childhood, innocence. Memories of before—before the darkness took everything."
  MinorCard Cups Seven -> "Choices, fantasy, illusion. Too many options, all of them traps. Choose wisely."
  MinorCard Cups Eight -> "Walking away, disillusionment, leaving behind. Sometimes the only win is escape."
  MinorCard Cups Nine -> "Contentment, satisfaction, wishes fulfilled. For one moment, you have enough."
  MinorCard Cups Ten -> "Harmony, alignment, family. The dream of peace in a city that devours dreams."
  MinorCard Cups Page -> "Curiosity, creative opportunity, new emotional experience. A young dreamer with visions."
  MinorCard Cups Knight -> "Romance, charm, imagination. Someone who follows their heart into danger."
  MinorCard Cups Queen -> "Compassion, intuition, emotional security. A healer in a city of wounds."
  MinorCard Cups King -> "Emotional balance, diplomacy, calm. A mediator who keeps the peace—for a price."

  -- Minor Arcana - Swords (Air: intellect, conflict, truth, pain)
  MinorCard Swords Ace -> "Clarity, breakthrough, new idea. The truth cuts through lies like a blade."
  MinorCard Swords Two -> "Difficult decisions, denial, stalemate. Two options, both terrible. Choose anyway."
  MinorCard Swords Three -> "Heartbreak, sorrow, grief. The wound that won't heal, the loss that defines."
  MinorCard Swords Four -> "Rest, recovery, contemplation. A moment to breathe before the next storm."
  MinorCard Swords Five -> "Conflict, defeat, winning at all costs. Victory that tastes like ash."
  MinorCard Swords Six -> "Transition, moving on, leaving troubles. Passage across dark waters to uncertain shores."
  MinorCard Swords Seven -> "Deception, strategy, cunning. In Doskvol, the clever survive. The honest don't."
  MinorCard Swords Eight -> "Imprisonment, restriction, self-limiting. Trapped by circumstances—or by your own choices."
  MinorCard Swords Nine -> "Anxiety, fear, nightmares. The thoughts that keep you awake in the eternal dark."
  MinorCard Swords Ten -> "Painful endings, betrayal, rock bottom. The knife in your back. The final blow."
  MinorCard Swords Page -> "Curiosity, vigilance, new ideas. A young spy with sharp ears and sharper tongue."
  MinorCard Swords Knight -> "Ambition, action, fast-thinking. Someone who cuts through problems—and people."
  MinorCard Swords Queen -> "Independence, perception, clear boundaries. A woman who sees through every lie."
  MinorCard Swords King -> "Authority, truth, intellectual power. A judge who renders verdicts in steel."

  -- Minor Arcana - Pentacles (Earth: material, practical, physical, wealth)
  MinorCard Pentacles Ace -> "New opportunity, prosperity, potential. Coin in hand, doors opening."
  MinorCard Pentacles Two -> "Balance, adaptability, juggling priorities. Two scores, one crew. Manage both."
  MinorCard Pentacles Three -> "Teamwork, collaboration, building. Your crew's skills combined make something greater."
  MinorCard Pentacles Four -> "Conservation, security, hoarding. Coin saved against the dark days coming."
  MinorCard Pentacles Five -> "Hardship, poverty, isolation. When the coin runs out, so do friends."
  MinorCard Pentacles Six -> "Generosity, charity, sharing wealth. Someone owes you—or you owe them."
  MinorCard Pentacles Seven -> "Patience, investment, long-term view. The score that pays off in years, not days."
  MinorCard Pentacles Eight -> "Apprenticeship, skill development, diligence. Learning the trade, one job at a time."
  MinorCard Pentacles Nine -> "Luxury, self-sufficiency, culmination. The good life—if you can keep it."
  MinorCard Pentacles Ten -> "Legacy, inheritance, establishment. Wealth that outlasts you. A dynasty in shadows."
  MinorCard Pentacles Page -> "Manifestation, new skill, scholarship. A young apprentice with golden hands."
  MinorCard Pentacles Knight -> "Efficiency, routine, responsibility. Someone who gets the job done, every time."
  MinorCard Pentacles Queen -> "Nurturing, abundance, security. A merchant queen who controls the flow of coin."
  MinorCard Pentacles King -> "Wealth, security, discipline. A crime lord who built an empire from nothing."

-- | Generate a description of a 3-card spread for the LLM
spreadDescription :: [TarotCard] -> Text
spreadDescription cards = case cards of
  [past, present, future] ->
    T.unlines
      [ "THE CARDS HAVE BEEN DRAWN:"
      , ""
      , "PAST - " <> cardName past
      , cardMeaning past
      , ""
      , "PRESENT - " <> cardName present
      , cardMeaning present
      , ""
      , "FUTURE - " <> cardName future
      , cardMeaning future
      , ""
      , "Let these cards guide the story's shape."
      ]
  _ -> T.unlines $ zipWith formatCard ([1..] :: [Int]) cards
  where
    formatCard n card =
      "Card " <> T.pack (show n) <> ": " <> cardName card <> "\n" <> cardMeaning card

-- ══════════════════════════════════════════════════════════════
-- ToGVal instances for Ginger templates
-- ══════════════════════════════════════════════════════════════

instance ToGVal m Suit where toGVal = genericToGVal
instance ToGVal m Rank where toGVal = genericToGVal
instance ToGVal m TarotCard where toGVal = genericToGVal
