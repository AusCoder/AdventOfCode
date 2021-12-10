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

possibilitiesByLength =
  foldr
    (\n m -> Map.insertWith (++) (length (convert n)) [n] m)
    Map.empty
    [0 .. 9]

difference s1 s2 = filter (not . (`elem` s2)) s1

union s1 s2 = Set.toList $ foldr Set.insert (foldr Set.insert Set.empty s1) s2

intersect s1 s2 = filter (`elem` s2) s1

headExact [x] = Just x
headExact _ = Nothing

calculateValue :: Entry -> Maybe Int
calculateValue (Entry signals outs) =
  let sigsOfLen n = filter ((==) n . length) signals

      sig1 = headExact $ sigsOfLen 2
      sig4 = headExact $ sigsOfLen 4
      sig7 = headExact $ sigsOfLen 3
      sig8 = headExact $ sigsOfLen 7
      sig6 =
        sig1 >>= \s1 ->
          headExact . filter ((==) 1 . length . intersect s1) $ sigsOfLen 6
      sig2 =
        sig4 >>= \s4 ->
          headExact . filter ((==) 2 . length . intersect s4) $ sigsOfLen 5
      sig9 =
        sig4 >>= \s4 ->
          headExact . filter ((==) 4 . length . intersect s4) $ sigsOfLen 6
      sig0 =
        sig6 >>= \s6 ->
          sig9 >>= \s9 ->
            headExact . filter (not . (`elem` [s6, s9])) $ sigsOfLen 6
      sig3 =
        sig1 >>= \s1 ->
          sig4 >>= \s4 ->
            headExact . filter ((==) 2 . length . intersect s1 . intersect s4) $ sigsOfLen 5
      sig5 =
        sig3 >>= \s3 ->
          sig2 >>= \s2 ->
            headExact . filter (not . (`elem` [s2, s3])) $ sigsOfLen 5

      insertMay k v m =
        fmap (\k' -> Map.insert k' v m) k

      sigsToDigitMay =
        insertMay sig6 6 Map.empty
          >>= insertMay sig8 8
          >>= insertMay sig7 7
          >>= insertMay sig4 4
          >>= insertMay sig1 1
          >>= insertMay sig2 2
          >>= insertMay sig9 9
          >>= insertMay sig0 0
          >>= insertMay sig3 3
          >>= insertMay sig5 5

      findDigit sigsToDigit sig =
        let sigAsSet = foldr Set.insert Set.empty
         in headExact
              . fmap snd
              . filter ((==) (sigAsSet sig) . sigAsSet . fst)
              . Map.toList
              $ sigsToDigit

      value = do
        sigsToDigit <- sigsToDigitMay
        digits <- mapM (findDigit sigsToDigit) outs
        return $
          foldr
            (\(n, x) acc -> 10 ^ (length outs - n) * x + acc)
            0
            $ zip [1 ..] digits
   in value

sumValues =
  maybeToErr "something went wrong, good luck with that"
    . fmap sum
    . mapM calculateValue

day8b = runDay "input/day8.txt" (parserManyLines parserEntry) sumValues

testEntry =
  Entry
    ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
    ["cdfeb", "fcadb", "cdfeb", "cdbaf"]

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
