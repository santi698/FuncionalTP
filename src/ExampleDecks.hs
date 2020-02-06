module ExampleDecks
  ( exampleDeck1
  , exampleDeck2
  )
where

import           Deck         (Deck (Deck))
import           ExampleCards

exampleDeck1 = Deck
  [ twoEyedNightmare
  , twoEyedNightmare
  , twoEyedNightmare
  , fireball
  , commonBat
  , commonBat
  , commonBat
  , explosiveGoblin
  , explosiveGoblin
  ]

exampleDeck2 = Deck
  [ braveSoldier
  , braveSoldier
  , braveSoldier
  , fireball
  , lazyCastle
  , lazyCastle
  , bigWall
  , bigWall
  , commonBat
  , commonBat
  , commonBat
  , explosiveGoblin
  , explosiveGoblin
  , boostMorale
  ]
