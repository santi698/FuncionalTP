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

import           Game (Card (MonsterCard, SpellCard), GameState (..),
                       Monster (..), Player (..), PlayerState (..), Spell (..),
                       raiseAttack)

newMonster :: String -> Int -> Int -> Card
newMonster name attack hp = MonsterCard $ Monster name attack hp False

newSpell :: String -> String -> (GameState -> GameState) -> Card
newSpell name description effect = SpellCard $ Spell name description effect

twoEyedNightmare = newMonster "Two-eyed nightmare" 2 1
lazyCastle = newMonster "Lazy Castle" 1 7
commonBat = newMonster "Common Bat" 1 1
braveSoldier = newMonster "Brave Soldier" 3 2
explosiveGoblin = newMonster "Explosive Goblin" 4 1
bigWall = newMonster "Big Wall" 1 10
fireball = newSpell "Fireball" "Deals 3 damage to the enemy" fireballEffect
fireballEffect gameState = case currentTurn gameState of
  Player1 ->
      gameState { player2State = p2s { playerHp = ((playerHp . player2State) gameState) - 3 }}
      where p2s = player2State gameState
  Player2 ->
      gameState { player1State = p1s { playerHp = ((playerHp . player2State) gameState) - 3 }}
      where p1s = player1State gameState
boostMorale = newSpell "Boost Morale" "Raises attack of all allied monsters by 1" boostMoraleEffect
boostMoraleEffect gameState = case currentTurn gameState of
  Player1 ->
    gameState { player1State = p1s { board = newBoard }}
    where p1s = player1State gameState
          newBoard = fmap (raiseAttack 1) $ (board . player1State) gameState
  Player2 ->
    gameState { player2State = p2s { board = newBoard } }
    where p2s = player2State gameState
          newBoard = fmap (raiseAttack 1) $ (board . player2State) gameState
