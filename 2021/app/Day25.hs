{-# LANGUAGE TupleSections #-}

module Day25 where

import Common
import qualified Data.Map.Strict as Map
import Debug.Trace
import Text.Parsec (char, choice, endOfLine, many1)

type Point = (Int, Int)

data Cucumber
  = EastCucumber
  | SouthCucumber
  deriving (Show, Eq)

type Positions = Map.Map Point Cucumber

data Puzzle = Puzzle Int Int Positions deriving (Show)

isEast EastCucumber = True
isEast SouthCucumber = False

isSouth = not . isEast

parserPuzzle :: StringParser Puzzle
parserPuzzle =
  let pCuc =
        choice
          [ [EastCucumber] <$ char '>',
            [SouthCucumber] <$ char 'v',
            [] <$ char '.'
          ]

      flattenCucs cs =
        let cucs = zip [0 ..] cs >>= \(x, cs') -> fmap (x,) cs'
         in (length cs, cucs)
      pRow = (flattenCucs <$> many1 pCuc) <* endOfLine

      aggregate :: [(Int, [(Int, Cucumber)])] -> Puzzle
      aggregate rows =
        let h = length rows
            w = fst (head rows)
            posns = zip [0 ..] (snd <$> rows) >>= \(y, row) -> fmap (\(x, c) -> ((x, y), c)) row
         in Puzzle w h (Map.fromList posns)
   in aggregate <$> many1 pRow

getDst :: Puzzle -> Point -> Cucumber -> Point
getDst (Puzzle w h _) (x, y) cuc =
  case cuc of
    EastCucumber -> ((x + 1) `mod` w, y)
    SouthCucumber -> (x, (y + 1) `mod` h)

doEpoch :: Puzzle -> Puzzle
doEpoch puzzle@(Puzzle w h positions) =
  let move posns p c =
        let d = getDst puzzle p c
         in if d `Map.member` posns then (p, c) else (d, c)
      souths = Map.filter isSouth positions
      positions' = foldr (uncurry Map.insert) souths . fmap (uncurry (move positions)) . Map.toList . Map.filter isEast $ positions
      easts = Map.filter isEast positions'
      positions'' = foldr (uncurry Map.insert) easts . fmap (uncurry (move positions')) . Map.toList . Map.filter isSouth $ positions'
   in Puzzle w h positions''

findStopPoint :: Puzzle -> Int
findStopPoint puzzle =
  let go n prev@(Puzzle _ _ prevPosns) cur@(Puzzle _ _ curPosns) =
        if prevPosns == curPosns then n else go (n + 1) cur (doEpoch cur)
   in go 1 puzzle (doEpoch puzzle)

day25a =
  runDay "input/day25.txt" parserPuzzle (Right . findStopPoint)
