module Day4 where

import Common
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import Debug.Trace
import Text.Parsec (char, count, endOfLine, many, optional, spaces)

type Board = HashMap.HashMap (Int, Int) Int

type PlayableBoard = HashMap.HashMap (Int, Int) (Int, Bool)

data Game = Game
  { nums :: [Int],
    boards :: [PlayableBoard]
  }
  deriving (Show)

parserNums = parserInt >>= \n -> (n :) <$> many (char ',' >> parserInt)

parserBoard :: StringParser Board
parserBoard =
  let parserLine = count 5 (spaces >> parserInt) >>= \ns -> ns <$ endOfLine
   in count 5 (zip [0 ..] <$> parserLine) >>= \pts ->
        return
          . foldr
            (\(x, y, n) m -> HashMap.insert (x, y) n m)
            HashMap.empty
          $ zip [0 ..] pts >>= \(y, xs) -> fmap (\(x, n) -> (y, x, n)) xs

parserGame = do
  ns <- parserNums
  endOfLine
  endOfLine
  bs <- many (parserBoard >>= \b -> b <$ optional endOfLine)
  let bs' = fmap (HashMap.map (\n -> (n, False))) bs
  return $ Game ns bs'

isBingo :: PlayableBoard -> Bool
isBingo b =
  let idxs = [0 .. 4]
      get x y = fromMaybe undefined . HashMap.lookup (x, y) $ b
      isHit x y = snd $ get x y
      checkRow y = all (`isHit` y) idxs
      checkCol x = all (isHit x) idxs
   in any checkRow idxs || any checkCol idxs

play n b =
  let ptMay = headMay . filter ((== n) . fst . snd) . HashMap.toList $ b
   in maybe b (\(pt, _) -> HashMap.insert pt (n, True) b) ptMay

calcScore n b = n * (sum . fmap fst . filter (not . snd) . HashMap.elems $ b)

playBingo :: Game -> Int
playBingo (Game (n : ns) bs) =
  let bs' = fmap (play n) bs
      bingo = headMay . filter isBingo $ bs'
   in maybe
        (playBingo (Game ns bs'))
        (calcScore n)
        bingo

day4a = runDay "input/day4.txt" parserGame (Right . playBingo)

looseBingo :: Game -> Int
looseBingo game =
  let go (Game (n : ns) bs) noBingos =
        let bs' = fmap (play n) bs
            noBingos' = filter (not . isBingo) bs'
         in if null noBingos'
              then calcScore n (play n $ head noBingos)
              else go (Game ns bs') noBingos'
   in go game (boards game)

day4b = runDay "input/day4.txt" parserGame (Right . looseBingo)
