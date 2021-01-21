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

intersection :: Box -> Box -> Maybe Box
intersection b1 b2 =
  let x = max (xMin b1) (xMin b2)
      y = max (yMin b1) (yMin b2)
      dx = abs $ x - min (xMax b1) (xMax b2)
      dy = abs $ y - min (yMax b1) (yMax b2)
   in if (dx > 0) && (dy > 0)
        then Just $ Box Nothing x y dx dy
        else Nothing

buildPointCount :: [Box] -> M.Map (Int, Int) Int
buildPointCount =
  let ptCount m [] = m
      ptCount m (box : rest) =
        let addOrInsert pt m' = M.insert pt (maybe 1 (1 +) $ M.lookup pt m') m'
            newM = foldr addOrInsert m $ points box
         in ptCount newM rest
   in ptCount M.empty

countIntersection :: [Box] -> Int
countIntersection = M.size . M.filter (> 1) . buildPointCount

_part1 :: T.Text -> Either AOCError Int
_part1 = fmap countIntersection . mapM (runSimpleParser boxParser) . T.lines

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

-- intersectionLenghtAtX :: M.Map Int [(Int, Bool)] -> Int -> Int
-- intersectionLenghtAtX yPointsByX x =
--   let yPoints = fromMaybe undefined $ M.lookup x yPointsByX

--       go inCount startPos acc [] = acc
--       go inCount startPos acc ((x, isEnd) : rest)
--         | inCount <= 1 = go (inCount + 1) x acc rest
--         | not isEnd = go (inCount + 1) x acc rest
--         | otherwise =
--           let newInCount = inCount - 1
--            in if newInCount == 1
--                 then
--                   let newAcc = (acc + x - startPos)
--                    in go newInCount undefined (trace (show newAcc) newAcc) rest
--                 else go newInCount startPos acc rest
--    in go 0 undefined 0 yPoints

intersectionLength :: [(Int, Bool)] -> Int
intersectionLength =
  let go inCount _ acc [] = acc
      go inCount Nothing acc ((x, False) : rest) = go (inCount + 1) (Just x) acc rest
      go inCount Nothing acc ((x, True) : rest) = go (inCount - 1) Nothing acc rest
      go inCount startM@(Just start) acc ((x, isEnd) : rest)
        | not isEnd = go (inCount + 1) startM acc rest
        | otherwise =
          let newInCount = inCount - 1
           in if newInCount == 1
                then go newInCount Nothing (acc + x - start) rest
                else go newInCount startM acc rest
   in go 0 Nothing 0

part1 :: T.Text -> Either AOCError Int
part1 = fmap intersectionArea . mapM (runSimpleParser boxParser) . T.lines

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
  let xs = sort $ [(0, 3), (1, 5)] >>= \(x, y) -> [(x, False), (y, True)]
  print $ xs
  print $ intersectionLength xs

-- print $ part1 content

-- print $ _part1 content

-- print $ part2 content

-- 0 3
--  1  5
--    4  6
