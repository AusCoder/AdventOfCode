module Day1 where

import Common

countIncreases :: [Int] -> Int
countIncreases depths =
  length . filter (< 0) . fmap (uncurry (-)) $ zip depths (tail depths)

day1a = runDay "input/day1.txt" (parserManyLines parserInt) (Right . countIncreases)

countWindowedIncreases :: [Int] -> Int
countWindowedIncreases depths =
  let sumTup (x, y, z) = x + y + z
      windowed = sumTup <$> zip3 depths (tail depths) (tail (tail depths))
   in length . filter (< 0) . fmap (uncurry (-)) $ zip windowed (tail windowed)

day1b = runDay "input/day1.txt" (parserManyLines parserInt) (Right . countWindowedIncreases)
