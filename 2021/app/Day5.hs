{-# LANGUAGE TupleSections #-}

module Day5 where

import Common
import qualified Data.HashMap.Strict as HashMap
import Text.Parsec (char, string)

data Line = Line (Int, Int) (Int, Int) deriving (Show)

parserLine = do
  x1 <- parserInt
  y1 <- char ',' >> parserInt
  string " -> "
  x2 <- parserInt
  y2 <- char ',' >> parserInt
  return $ Line (x1, y1) (x2, y2)

isHorizOrVert (Line (x1, y1) (x2, y2)) = (x1 == x2) || (y1 == y2)

points (Line (x1, y1) (x2, y2))
  | x1 == x2 = fmap (x1,) [(min y1 y2) .. (max y1 y2)]
  | x1 > x2 = points (Line (x2, y2) (x1, y1))
  | otherwise =
    let ln x = ((y2 - y1) `div` (x2 - x1)) * (x - x1) + y1
     in fmap (\x -> (x, ln x)) [x1 .. x2]

countOverlaps :: [Line] -> Int
countOverlaps lns =
  let pts = lns >>= points
      counts = foldr (\p m -> HashMap.insertWith (+) p 1 m) HashMap.empty pts
   in length . filter (>= 2) . HashMap.elems $ counts

day5a =
  runDay
    "input/day5.txt"
    (parserManyLines parserLine)
    (Right . countOverlaps . filter isHorizOrVert)

day5b =
  runDay
    "input/day5.txt"
    (parserManyLines parserLine)
    (Right . countOverlaps)
