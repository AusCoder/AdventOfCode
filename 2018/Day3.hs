{-# LANGUAGE OverloadedStrings #-}

module Day3 (day3) where

import Common
import qualified Data.Map.Strict as M
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

boxParser :: SimpleParser Box
boxParser = do
  id <- PC.char '#' >> numberParser
  x <- PC.spaces >> PC.char '@' >> PC.spaces >> numberParser
  y <- PC.char ',' >> numberParser
  w <- PC.char ':' >> PC.spaces >> numberParser
  h <- PC.char 'x' >> numberParser
  return $ Box (Just id) x y w h

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

part1 :: T.Text -> Either AOCError Int
part1 = fmap countIntersection . mapM (runSimpleParser boxParser) . T.lines

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
