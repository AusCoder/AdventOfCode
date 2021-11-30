{-# LANGUAGE TupleSections #-}

module Day2 where

import Common
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Parsec

parserStrs :: StringParser [String]
parserStrs = many (many alphaNum >>= \x -> x <$ endOfLine)

letterCounts :: String -> Map.Map Char Int
letterCounts =
  let foldFn c acc =
        let preVal = fromMaybe 0 $ Map.lookup c acc
         in Map.insert c (preVal + 1) acc
   in foldr foldFn Map.empty

part1 :: [String] -> Int
part1 strs =
  let counts = map letterCounts strs
      count2 = length $ filter (elem 2) counts
      count3 = length $ filter (elem 3) counts
   in count2 * count3

day2a = runDay "input/day2.txt" parserStrs (Right . part1)

letterDistance :: String -> String -> Either AOCError Int
letterDistance s1 s2 =
  if length s1 /= length s2
    then Left (AOCCustomError "strings different length")
    else Right . length . filter (uncurry (==)) $ zip s1 s2

part2 :: [String] -> Either AOCError String
part2 strs =
  let count s1 s2 = letterDistance s1 s2 >>= Right . (,s1,s2)
      differBy1 (n, s1, _) = n == length s1 - 1
      common s1 s2 = filter (uncurry (==)) $ zip s1 s2
   in do
        counts <- mapM (uncurry count) $ pairs strs
        (_, s1, s2) <-
          maybeToErr "couldnt find head" . headMay $
            filter differBy1 counts
        return . map fst $ common s1 s2

-- day2b = runDay "input/day2.txt" parserStrs part2

day2b = runDay "input/day2.txt" parserStrs (Right . length . pairs)
