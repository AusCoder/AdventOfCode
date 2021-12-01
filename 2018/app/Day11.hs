{-# LANGUAGE TupleSections #-}

module Day11 where

import Common
import qualified Data.HashMap.Strict as HashMap
import Data.List (maximumBy)
import Data.Ord (comparing)
import Debug.Trace

type Point = (Integer, Integer)

calculatePowerLevel :: Integer -> Point -> Integer
calculatePowerLevel serialNum (x, y) =
  let rackId = x + 10
      calcHundDigit n = n `div` 100 `mod` 10
   in calcHundDigit ((rackId * y + serialNum) * rackId) - 5

getFuelCells :: Integer -> [Point]
getFuelCells n = do
  x <- [1 .. n]
  y <- [1 .. n]
  return (x, y)

getAdjacentPoints :: Integer -> Point -> [Point]
getAdjacentPoints n (x, y) = do
  dx <- [0 .. (n - 1)]
  dy <- [0 .. (n - 1)]
  return (x + dx, y + dy)

calculatePowerLevels :: Integer -> Integer -> HashMap.HashMap Point Integer
calculatePowerLevels gridSize serialNum =
  foldr
    (\p m -> HashMap.insert p (calculatePowerLevel serialNum p) m)
    HashMap.empty
    $ getFuelCells gridSize

calculateMax3x3Square :: Integer -> Integer -> Either AOCError (Point, Integer)
calculateMax3x3Square gridSize serialNum =
  let powerLevels = calculatePowerLevels gridSize serialNum

      calc3x3Square :: Point -> Either AOCError (Point, Integer)
      calc3x3Square p =
        let adjPts = getAdjacentPoints 3 p
            lvlAt x = maybeToErr ("couldnt find power level at point:" ++ show x) (x `HashMap.lookup` powerLevels)
         in (p,) . sum <$> mapM lvlAt adjPts

      pointsToCheck = getFuelCells (gridSize - 2)
   in maximumBy (comparing snd) <$> mapM calc3x3Square pointsToCheck

day11a = printResult $ calculateMax3x3Square 300 9445

-- this is slow
-- needs a better caching strategy or a way to kull the branches
-- need to think about negative squares down the line
calculateMaxSquare :: Integer -> Integer -> Either AOCError ((Integer, Integer, Integer), Integer)
calculateMaxSquare gridSize serialNum =
  let powerLevels = calculatePowerLevels gridSize serialNum

      getLvl lvls p = maybeToErr ("couldnt find power level at point:" ++ show p) (p `HashMap.lookup` lvls)

      -- Eg: (1,1) -> (1,2), (2,1), (2,2)
      getBoxExtraPts :: Integer -> Point -> [Point]
      getBoxExtraPts boxSize (x, y) =
        ((x + boxSize - 1,) <$> [y .. y + boxSize - 1])
          ++ ((,y + boxSize - 1) <$> [x .. x + boxSize - 2])

      -- for each pointToCheck: getLevelSoFar + sum (extras pts at this boxSize)
      calcPowerLevelsAtBoxSize :: HashMap.HashMap Point Integer -> Integer -> [Point] -> Either AOCError [(Point, Integer)]
      calcPowerLevelsAtBoxSize cachedLvls boxSize pointsToCheck =
        let calcFromCache p = do
              prev <- getLvl cachedLvls p
              extraThisLvl <- fmap sum . mapM (getLvl powerLevels) . getBoxExtraPts boxSize $ p
              -- trace ("boxSize: " ++ show boxSize ++ " extra pts: " ++ show (getBoxExtraPts boxSize p)) $
              return (p, prev + extraThisLvl)
         in if boxSize == 1
              then mapM (\p -> (p,) <$> getLvl powerLevels p) pointsToCheck
              else mapM calcFromCache pointsToCheck

      go maxSoFar cachedLvls boxSize =
        let pointsToCheck = getFuelCells (gridSize - boxSize + 1)
         in if null pointsToCheck
              then return maxSoFar
              else
                calcPowerLevelsAtBoxSize cachedLvls boxSize pointsToCheck >>= \lvlsThisSize ->
                  let cachedLvls' = foldr (uncurry HashMap.insert) cachedLvls lvlsThisSize
                      ((maxX, maxY), maxLvl) = maximumBy (comparing snd) lvlsThisSize
                      maxSoFar' = case maxSoFar of
                        Nothing -> Just ((maxX, maxY, boxSize), maxLvl)
                        Just (prevMaxPt, prevMaxLvl) ->
                          if maxLvl > prevMaxLvl then Just ((maxX, maxY, boxSize), maxLvl) else Just (prevMaxPt, prevMaxLvl)
                   in trace ("max at lvl point: " ++ show ((maxX, maxY, boxSize), maxLvl) ++ " max so far: " ++ show maxSoFar') $
                        go maxSoFar' cachedLvls' (boxSize + 1)
   in go Nothing HashMap.empty 1 >>= maybeToErr "couldn't find max"

day11b = calculateMaxSquare 300 9445
