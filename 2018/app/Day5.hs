module Day5 where

import Common
import Data.Char (isLower, isUpper, toLower)
import qualified Data.Set as Set
import Text.Parsec (alphaNum, many)

parserPolymer :: StringParser String
parserPolymer = many alphaNum

react :: String -> String
react polymer =
  let go front [] = front
      go front [x] = go (x : front) []
      go front (x : y : back) =
        if toLower x == toLower y && ((isLower x && isUpper y) || (isUpper x && isLower y))
          then case front of
            [] -> go [] back
            (f : front') -> go front' (f : back)
          else go (x : front) (y : back)
   in go "" polymer

day5a = runDay "input/day5.txt" parserPolymer (Right . length . react)

smallestAfterRemove :: String -> Int
smallestAfterRemove polymer =
  let uniqElems = Set.toList $ foldr (Set.insert . toLower) Set.empty polymer
      lengthWithoutElem x = length . react . filter ((/=) x . toLower) $ polymer
   in minimum $ lengthWithoutElem <$> uniqElems

day5b = runDay "input/day5.txt" parserPolymer (Right . smallestAfterRemove)
