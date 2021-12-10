module Day8 where

import Common
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec (alphaNum, char, count, many1, space)

data Entry = Entry [String] [String] deriving (Show)

parserEntry = do
  signals <- count 10 (many1 alphaNum >>= \s -> s <$ space)
  char '|'
  outputs <- count 4 (space >> many1 alphaNum)
  return $ Entry signals outputs

countOutputValues :: [Entry] -> Int
countOutputValues entries =
  let lengthsToCount = [2, 4, 3, 7]
      count (Entry _ outs) = length . filter (`elem` lengthsToCount) . fmap length $ outs
   in sum . fmap count $ entries

day8a = runDay "input/day8.txt" (parserManyLines parserEntry) (Right . countOutputValues)

convert 0 = "abcefg"
convert 1 = "cf"
convert 2 = "acdeg"
convert 3 = "acdfg"
convert 4 = "bcdf"
convert 5 = "abdfg"
convert 6 = "abdfeg"
convert 7 = "acf"
convert 8 = "abcdefg"
convert 9 = "abcdfg"
convert _ = error "uh oh"

difference s1 s2 = filter (not . (`elem` s2)) s1

union s1 s2 = Set.toList $ foldr Set.insert (foldr Set.insert Set.empty s1) s2

intersect s1 s2 = filter (`elem` s2) s1

decode :: Entry -> String
decode entry =
  let go unknownMappings =
        undefined
   in go Map.empty

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....

--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
