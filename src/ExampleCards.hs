module ExampleCards
  ( twoEyedNightmare
  , lazyCastle
  , commonBat
  , braveSoldier
  , explosiveGoblin
  , fireball
  , boostMorale
  , bigWall
  )
where

import           Card (Card (Magic, Monster), MagicCard (..), MonsterCard (..),
                       raiseAttack)

newMonster :: String -> Int -> Int -> Card
newMonster name attack hp = Monster $ MonsterCard name attack hp False

newMagic :: String -> String -> Card
newMagic name description = Magic $ MagicCard name description

twoEyedNightmare = newMonster "Two-eyed nightmare" 2 1
lazyCastle = newMonster "Lazy Castle" 1 7
commonBat = newMonster "Common Bat" 1 1
braveSoldier = newMonster "Brave Soldier" 3 2
explosiveGoblin = newMonster "Explosive Goblin" 4 1
bigWall = newMonster "Big Wall" 1 10
fireball = newMagic "Fireball" "Deals 3 damage to the enemy"
boostMorale = newMagic "Boost Morale" "Raises attack of all allied monsters by 1"
