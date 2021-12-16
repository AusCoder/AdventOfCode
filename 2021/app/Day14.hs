module Day14 where

import Common
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace
import Text.Parsec (endOfLine, many1, string, upper)

type Rules = HashMap.HashMap (Char, Char) Char

data Puzzle = Puzzle String Rules

parserPuzzle :: StringParser Puzzle
parserPuzzle =
  let parserRule = do
        c1 <- upper
        c2 <- upper
        string " -> "
        r <- upper
        endOfLine
        return ((c1, c2), r)
   in do
        polymer <- many1 upper
        endOfLine >> endOfLine
        rulePairs <- many1 parserRule
        let rules = foldr (\(p, r) acc -> HashMap.insert p r acc) HashMap.empty rulePairs
        return $ Puzzle polymer rules

growPolymer numEpochs (Puzzle polymer rules) =
  let go xs [] = return . reverse $ xs
      go (x : xs) (y : ys) =
        maybeToErr "no rule" (HashMap.lookup (x, y) rules) >>= \c ->
          go (y : c : x : xs) ys

      go' _ [] = Left . AOCCustomError $ "polymer has no head"
      go' 0 xs = return xs
      go' n (x : xs) =
        go [x] xs >>= go' (n - 1)
   in go' numEpochs polymer

calculateDifference :: Integer -> Puzzle -> Either AOCError Integer
calculateDifference numEpochs puzzle@(Puzzle _ rules) =
  let puzzle' = Puzzle "NN" rules
      calcCounts =
        foldr
          (\c acc -> HashMap.insertWith (+) c 1 acc)
          HashMap.empty

      counts = calcCounts <$> growPolymer numEpochs puzzle
   in counts >>= \c -> return $ maximum c - minimum c

day14a =
  runDay
    "input/day14-test.txt"
    parserPuzzle
    (calculateDifference 10)

-- count the pairs...

growCounts numEpochs (Puzzle polymer rules) =
  let transformPair pair@(x, y) n =
        (\z -> ((x, z), (z, y), n)) <$> maybeToErr "no rule" (HashMap.lookup pair rules)

      doEpoch pairCounts letterCounts = do
        transformedPairs <- mapM (uncurry transformPair) . HashMap.toList $ pairCounts
        let pairCounts' =
              foldr
                ( \(p1, p2, n) acc ->
                    HashMap.insertWith (+) p1 n $
                      HashMap.insertWith (+) p2 n acc
                )
                HashMap.empty
                transformedPairs
            letterCounts' =
              foldr
                (\((_, z), _, n) acc -> HashMap.insertWith (+) z n acc)
                letterCounts
                transformedPairs
        return (pairCounts', letterCounts')

      go 0 pairCounts letterCounts = return letterCounts
      go n pairCounts letterCounts = doEpoch pairCounts letterCounts >>= uncurry (go (n - 1))

      initPairCounts =
        foldr (\p acc -> HashMap.insertWith (+) p 1 acc) HashMap.empty $
          zip polymer (tail polymer)
      initLetterCounts = foldr (\c acc -> HashMap.insertWith (+) c 1 acc) HashMap.empty polymer
   in go numEpochs initPairCounts initLetterCounts

calculateDifferenceCounts :: Integer -> Puzzle -> Either AOCError Integer
calculateDifferenceCounts numEpochs puzzle =
  growCounts numEpochs puzzle >>= \counts ->
    return $ maximum counts - minimum counts

day14b =
  runDay
    "input/day14.txt"
    parserPuzzle
    (calculateDifferenceCounts 40)
