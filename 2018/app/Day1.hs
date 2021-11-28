module Day1 where

import Common
import qualified Data.Set as Set
import Text.Parsec

parserInts :: StringParser [Int]
parserInts = many (parserSignedInt >>= \x -> x <$ endOfLine)

partialSums :: [Int] -> [Int]
partialSums nums =
  let f acc [] = []
      f acc (x : xs) = (x + acc) : f (x + acc) xs
   in f 0 nums

firstRepeat :: [Int] -> Maybe Int
firstRepeat =
  let f acc [] = Nothing
      f acc (x : xs) =
        if x `Set.member` acc then Just x else f (Set.insert x acc) xs
   in f Set.empty

findRepeat =
  maybeToErr "no repeat found" . firstRepeat . partialSums . cycle

day1a = runDayWithParser "input/day1.txt" parserInts (Right . sum)

day1b = runDayWithParser "input/day1.txt" parserInts findRepeat
