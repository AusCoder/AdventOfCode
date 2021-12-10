{-# LANGUAGE TupleSections #-}

module Day9 where

import Common
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Sort (sortOn)
import Debug.Trace
import Text.Parsec (digit, endOfLine, many1)

type Point = (Int, Int)

type HeightMap = HashMap.HashMap Point Int

parserHeightMap :: StringParser HeightMap
parserHeightMap =
  many1 (many1 (fmap digitToInt digit) >>= \ln -> ln <$ endOfLine) >>= \pts ->
    return $
      foldr
        ( \(y, pts') acc ->
            foldr
              (\(x, p) acc' -> HashMap.insert (y, x) p acc')
              acc
              $ zip [0 ..] pts'
        )
        HashMap.empty
        $ zip [0 ..] pts

adjacentPoints (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isLowPoint :: HeightMap -> Point -> Int -> Bool
isLowPoint heightMap pt value =
  let adjValues =
        adjacentPoints pt
          >>= maybeToList . (`HashMap.lookup` heightMap)
   in all (value <) adjValues

calculateRiskLevel :: HeightMap -> Int
calculateRiskLevel heightMap =
  sum
    . fmap ((+) 1 . snd)
    . filter (uncurry (isLowPoint heightMap))
    . HashMap.toList
    $ heightMap

day9a =
  runDay
    "input/day9.txt"
    parserHeightMap
    (Right . calculateRiskLevel)

type Basin = Set.Set Point

findBasinContainingPoint :: HeightMap -> Point -> Basin
findBasinContainingPoint heightMap startPoint =
  let go [] basin = basin
      go (curPoint : ptsToExplore) basin =
        let basin' = Set.insert curPoint basin

            adjPtsWithValue =
              adjacentPoints curPoint >>= \pt ->
                maybeToList . fmap (pt,) $ HashMap.lookup pt heightMap

            ptsToAdd =
              filter (not . (`elem` basin'))
                . fmap fst
                . filter ((< 9) . snd)
                $ adjPtsWithValue

            ptsToExplore' = foldr (:) ptsToExplore ptsToAdd
         in if curPoint `Set.member` basin
              then go ptsToExplore basin
              else go ptsToExplore' basin'
   in go [startPoint] Set.empty

findBasins :: HeightMap -> [Basin]
findBasins heightMap =
  let foldFn (pt, val) basins =
        if val < 9 && not (any (Set.member pt) basins)
          then findBasinContainingPoint heightMap pt : basins
          else basins
   in foldr foldFn [] (HashMap.toList heightMap)

calculateBasinProduct :: HeightMap -> Int
calculateBasinProduct heightMap =
  let basins = findBasins heightMap
   in product . take 3 . sortOn negate . fmap Set.size $ basins

day9b =
  runDay
    "input/day9.txt"
    parserHeightMap
    (Right . calculateBasinProduct)
