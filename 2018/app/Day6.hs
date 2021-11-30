{-# LANGUAGE TupleSections #-}

module Day6 where

import Common
import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Text.Parsec

type Point = (Int, Int)

parserPoint :: StringParser Point
parserPoint =
  parserInt >>= \x ->
    char ',' >> space >> parserInt >>= \y -> return (x, y)

parserPoints = many (parserPoint >>= \p -> p <$ endOfLine)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

closestUniqPoint :: [Point] -> Point -> Maybe Point
closestUniqPoint points p =
  let go _ closest [] = closest
      go Nothing _ (x : xs) = go (Just $ distance p x) (Just x) xs
      go (Just curClosestDist) curUniq (x : xs) =
        let d = distance p x
         in if d < curClosestDist
              then go (Just d) (Just x) xs
              else
                if d == curClosestDist
                  then go (Just d) Nothing xs
                  else go (Just curClosestDist) curUniq xs
   in go Nothing Nothing points

getSpan :: [Point] -> [Point]
getSpan points = do
  x <- [minimum (fst <$> points) .. maximum (fst <$> points)]
  y <- [minimum (snd <$> points) .. maximum (snd <$> points)]
  return (x, y)

getEdgePts :: [Point] -> Set.Set Point
getEdgePts points =
  let xMin = minimum (fst <$> points)
      xMax = maximum (fst <$> points)
      yMin = minimum (snd <$> points)
      yMax = maximum (snd <$> points)
   in foldr Set.insert Set.empty $
        map (,yMin) [xMin .. xMax]
          ++ map (,yMax) [xMin .. xMax]
          ++ map (xMin,) [yMin + 1 .. yMax - 1]
          ++ map (xMax,) [yMin + 1 .. yMax - 1]

-- filter base points by those that dont have a matching
-- edge point
-- for remaining base points, find the largest area
largestFiniteArea :: [Point] -> Int
largestFiniteArea points =
  let foldFn p m =
        maybe m (\closestP -> Map.insertWith (++) closestP [p] m) $
          closestUniqPoint points p
      closestPts = foldr foldFn Map.empty $ getSpan points
      edgePts = getEdgePts points
      finitePts = Map.filter (not . any (`Set.member` edgePts)) closestPts
   in maximum . fmap snd . Map.toList . Map.map length $ finitePts

day6a = runDay "input/day6.txt" parserPoints (Right . largestFiniteArea)

distanceToAllPoints points p = sum . fmap (distance p) $ points

getAdjacentPoints :: Point -> [Point]
getAdjacentPoints (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

countPointsCloseToAll :: Int -> [Point] -> Int
countPointsCloseToAll distanceThreshold points =
  let startPt =
        head . filter ((>) distanceThreshold . distanceToAllPoints points) $
          getSpan points

      -- walks a graph to count points close to all points
      go [] _ acc = acc
      go (x : stack) seen acc =
        if Set.member x seen
          then go stack seen acc
          else
            let dist = distanceToAllPoints points x
                acc' = if dist < distanceThreshold then acc + 1 else acc
                seen' = Set.insert x seen
                stack' =
                  if dist >= distanceThreshold + 2
                    then stack
                    else
                      foldr (:) stack
                        . filter (not . flip Set.member seen)
                        . getAdjacentPoints
                        $ x
             in go stack' seen' acc'
   in go [startPt] Set.empty 0

day6b = runDay "input/day6.txt" parserPoints (Right . countPointsCloseToAll 10000)
