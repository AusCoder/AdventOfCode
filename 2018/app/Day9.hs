module Day9 where

import Common
import Control.Monad.State
import qualified Data.Map.Strict as Map

data MarbleCircle = MarbleCircle
  { left :: [Int],
    right :: [Int]
  }
  deriving (Show)

createMarbleCircle = MarbleCircle [0] []

shiftRight marbleCircle =
  case left marbleCircle of
    [] -> marbleCircle
    (x : xs) -> shiftRight $ MarbleCircle xs (x : right marbleCircle)

shiftLeft marbleCircle =
  case right marbleCircle of
    [] -> marbleCircle
    (x : xs) -> shiftLeft $ MarbleCircle (x : left marbleCircle) xs

ensureHasRight marbleCircle =
  case right marbleCircle of
    [] -> shiftRight marbleCircle
    (x : _) -> marbleCircle

ensureHasLeft marbleCircle =
  case left marbleCircle of
    [] -> shiftLeft marbleCircle
    (x : _) -> marbleCircle

insertMarble n marbleCircle =
  let marbleCircle' = ensureHasRight marbleCircle
   in case right marbleCircle' of
        (x : xs) -> MarbleCircle (n : x : left marbleCircle') xs
        [] -> undefined

removeMarble marbleCircle =
  let go 0 marbleCircle =
        let marbleCircle' = ensureHasLeft marbleCircle
         in case left marbleCircle' of
              (x : xs) ->
                let marbleCircle'' = ensureHasRight $ MarbleCircle xs (right marbleCircle')
                 in case right marbleCircle'' of
                      (y : ys) -> (x, MarbleCircle (y : left marbleCircle'') ys)
                      [] -> undefined
              [] -> undefined
      go n marbleCircle =
        let marbleCircle' = ensureHasLeft marbleCircle
         in case left marbleCircle' of
              (x : xs) -> go (n - 1) (MarbleCircle xs (x : right marbleCircle'))
              [] -> undefined
   in go 7 marbleCircle

data Game = Game
  { marbleCircle :: MarbleCircle,
    playerScores :: Map.Map Int Int
  }
  deriving (Show)

playGame playerCount lastMarble =
  let playMarble :: Int -> Int -> State Game ()
      playMarble playerId marbleValue =
        if marbleValue `mod` 23 == 0
          then
            get >>= \s ->
              let (x, c') = removeMarble $ marbleCircle s
                  scores = Map.insertWith (+) playerId (x + marbleValue) $ playerScores s
               in put $ Game c' scores
          else modify $ \s ->
            s {marbleCircle = insertMarble marbleValue (marbleCircle s)}

      (_, finalState) =
        flip runState (Game createMarbleCircle Map.empty) $
          mapM_ (uncurry playMarble) $
            zip (cycle [1 .. playerCount]) [1 .. lastMarble]
   in maximum . Map.elems . playerScores $ finalState

playerCount' :: Int
-- playerCount' = 9
playerCount' = 463

lastMarble' :: Int
-- lastMarble' = 25
lastMarble' = 71787

lastMarble'' :: Int
lastMarble'' = 71787 * 100

day9a = playGame playerCount' lastMarble'

day9b = playGame playerCount' lastMarble''
