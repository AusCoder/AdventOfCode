{-# LANGUAGE TupleSections #-}

module Day15 where

import Common
import Data.Char (digitToInt)
import Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as HashMap
import Data.List (minimumBy)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Debug.Trace
import Text.Parsec (digit, endOfLine, many1)

type Point = (Int, Int)

type MapPointInt = HashMap.HashMap Point Int

parserCounts :: StringParser MapPointInt
parserCounts =
  many1 pLn >>= \lns ->
    return $
      HashMap.fromList
        (zip [0 ..] lns >>= \(y, ln) -> (\(x, n) -> ((x, y), n)) <$> ln)
  where
    pLn = zip [0 ..] <$> (many1 (digitToInt <$> digit) <* endOfLine)

bottomRight weights =
  let pts = HashMap.keys weights
   in (maximum (fst <$> pts), maximum (snd <$> pts))

distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

findSmallest :: Point -> MapPointInt -> MapPointInt -> MapPointInt -> Int
findSmallest target weights known frontier =
  case known !? target of
    Just n -> n
    Nothing ->
      let (p, v) =
            minimumBy (comparing snd) $
              HashMap.toList frontier

          known' = HashMap.insert p (v - distance p target) known

          frontier' =
            foldr
              (\(p, v) acc -> HashMap.insertWith min p v acc)
              (HashMap.delete p frontier)
              $ neighbors target weights known' p
       in findSmallest target weights known' frontier'

adjacentPts (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- The fact that we add the 'distance adj target' here makes it
-- the difference between Dijkstras and the A* algorithms.
-- Really this just encourages us to search paths closer to
-- target before paths that are further from the target.
neighbors target weights known point =
  maybeToList (known !? point) >>= \value ->
    adjacentPts point >>= \adj ->
      if adj `HashMap.member` known
        then []
        else case weights !? adj of
          Nothing -> []
          Just adjWght -> [(adj, value + adjWght + distance adj target)]

smallestRisk :: MapPointInt -> Int
smallestRisk weights =
  let target = bottomRight weights
      initKnown = HashMap.fromList [((0, 0), 0)]
      initFrontierMap = HashMap.fromList $ neighbors target weights initKnown (0, 0)
   in findSmallest target weights initKnown initFrontierMap

day15a =
  runDay
    "input/day15.txt"
    parserCounts
    (Right . smallestRisk)

expand weights =
  let (b, l) = bottomRight weights
      w = b + 1
      h = l + 1
   in foldr
        ( \(fx, fy, f) acc ->
            foldr
              ( \((x, y), v) acc' -> HashMap.insert (x + fx * w, y + fy * h) (((v + f - 1) `mod` 9) + 1) acc'
              )
              acc
              $ HashMap.toList weights
        )
        weights
        $ [0 .. 4] >>= \y -> fmap (\x -> (x, y, x + y)) [0 .. 4]

smallestRiskLargeGrid :: MapPointInt -> Int
smallestRiskLargeGrid weights = smallestRisk $ expand weights

day15b =
  runDay
    "input/day15.txt"
    parserCounts
    (Right . smallestRiskLargeGrid)
