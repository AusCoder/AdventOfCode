{-# LANGUAGE OverloadedStrings #-}

module Day3 (day3) where

import Common
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Sort (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Safe
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

-- I can do a much better sweepig algorithm than what is
-- used here.

data Box = Box
  { boxId :: Maybe Int,
    xMin :: Int,
    yMin :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show)

boxParser :: SimpleParser Box
boxParser = do
  id <- PC.char '#' >> numberParser
  x <- PC.spaces >> PC.char '@' >> PC.spaces >> numberParser
  y <- PC.char ',' >> numberParser
  w <- PC.char ':' >> PC.spaces >> numberParser
  h <- PC.char 'x' >> numberParser
  return $ Box (Just id) x y w h

xMax b = xMin b + width b

yMax b = yMin b + height b

points :: Box -> [(Int, Int)]
points (Box _ x y w h) = do
  s <- [x .. x + w - 1]
  t <- [y .. y + h - 1]
  return (s, t)

buildPointCount :: [Box] -> M.Map (Int, Int) Int
buildPointCount =
  let ptCount m [] = m
      ptCount m (box : rest) =
        let addOrInsert pt m' = M.insert pt (maybe 1 (1 +) $ M.lookup pt m') m'
            newM = foldr addOrInsert m $ points box
         in ptCount newM rest
   in ptCount M.empty

-- algo idea:
--    fold over the sorted x points
--    for each x point
--      find the intersection length
intersectionArea :: [Box] -> Int
intersectionArea boxes =
  let xPoints = sort $ boxes >>= \b -> [xMin b, xMax b]
      boxContainsX x (Box _ a _ w _) = a <= x && x < (a + w)
      getYPointsAtX x = sort $ filter (boxContainsX x) boxes >>= \b -> [(yMin b, False), (yMax b, True)]
      yPointsByX = foldr (\x m -> M.insert x (getYPointsAtX x) m) M.empty xPoints
      -- lookup X, calculate intersection length add to acc
      unsafeLookup x = fromMaybe undefined $ M.lookup x yPointsByX
      foldFn (a, b) acc = acc + (b - a) * intersectionLength (unsafeLookup a)
   in foldr foldFn 0 $ zip xPoints (tail xPoints)

intersectionLength :: [(Int, Bool)] -> Int
intersectionLength =
  let go inCount startM acc [] = acc
      go inCount startM acc ((x, isEnd) : rest) =
        let inCount' = if isEnd then inCount - 1 else inCount + 1
         in case (inCount, isEnd) of
              (1, False) -> go inCount' (Just x) acc rest
              (2, True) -> go inCount' Nothing (acc + x - fromMaybe undefined startM) rest
              _ -> go inCount' startM acc rest
   in go 0 Nothing 0

part1 :: T.Text -> Either AOCError Int
part1 = fmap intersectionArea . mapM (runSimpleParser boxParser) . T.lines

-- looking at all points is probably not needed here
-- we can sweep left to right, then sweep boxes top
-- to bottom, keep track of whether 2 boxes intersect each other
findUncoveredBox :: [Box] -> Either AOCError Int
findUncoveredBox boxes =
  let ptCount = buildPointCount boxes
      isNotCovered box = all (\p -> M.lookup p ptCount == Just 1) (points box)
      uncoveredBoxM = headMay . filter isNotCovered $ boxes
      uncoveredBoxE = maybe (Left $ AOCGenericError "No uncovered boxes") Right uncoveredBoxM
      getId = maybe (Left $ AOCGenericError "Box has no id") Right . boxId
   in uncoveredBoxE >>= getId

part2 :: T.Text -> Either AOCError Int
part2 t = mapM (runSimpleParser boxParser) (T.lines t) >>= findUncoveredBox

day3 :: IO ()
day3 = do
  content <- TIO.readFile "input/day3.txt"
  print $ part1 content
  print $ part2 content
