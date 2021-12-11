module Day11 where

import Common
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec (digit, many)

type Octopuses = HashMap.HashMap (Int, Int) Int

parserDumboOctopuses :: StringParser Octopuses
parserDumboOctopuses =
  parserManyLines (many digit) >>= \lvls ->
    return $
      foldr
        ( \(y, lvls') acc ->
            foldr
              (\(x, v) acc' -> HashMap.insert (y, x) (digitToInt v) acc')
              acc
              $ zip [0 ..] lvls'
        )
        HashMap.empty
        $ zip [0 ..] lvls

adjacentPoints (y, x) =
  [(y - 1, x - n) | n <- [-1 .. 1]]
    ++ [(y + 1, x - n) | n <- [-1 .. 1]]
    ++ [(y, x - 1), (y, x + 1)]

runStep :: Octopuses -> (Octopuses, Int)
runStep octopuses =
  let octopuses' = HashMap.map (+ 1) octopuses
      getFlashPts = HashMap.keys . HashMap.filter (> 9)

      -- for each flashPts, for each neighbor, increment
      doFlashes octs [] seenPts = (octs, seenPts)
      doFlashes octs (flashPt : flashPts) seenPts
        | flashPt `Set.member` seenPts = doFlashes octs flashPts seenPts
        | otherwise =
          let seenPts' = Set.insert flashPt seenPts
              octs' = foldr (HashMap.alter (fmap (+ 1))) octs $ adjacentPoints flashPt
              newFlashPts = filter (not . (`Set.member` seenPts')) . getFlashPts $ octs'
              flashPts' = foldr (:) flashPts newFlashPts
           in doFlashes octs' flashPts' seenPts'

      (octopuses'', seenFlashPts) = doFlashes octopuses' (getFlashPts octopuses') Set.empty
      resetToZero n = if n > 9 then 0 else n
      octopuses''' =
        foldr
          (HashMap.alter (fmap resetToZero))
          octopuses''
          seenFlashPts
   in (octopuses''', Set.size seenFlashPts)

countFlashes :: Int -> Octopuses -> Int
countFlashes numSteps octopuses =
  let go 0 acc _ = acc
      go n acc octs =
        let (octs', numFlashes) = runStep octs
         in go (n - 1) (acc + numFlashes) octs'
   in go numSteps 0 octopuses

day11a =
  runDay
    "input/day11.txt"
    parserDumboOctopuses
    (Right . countFlashes 100)

findFirstSyncronisedFlash :: Octopuses -> Int
findFirstSyncronisedFlash octopuses =
  let go octs n =
        let (octs', numFlashes) = runStep octs
         in if numFlashes == HashMap.size octs
              then n
              else go octs' (n + 1)
   in go octopuses 1

day11b =
  runDay
    "input/day11.txt"
    parserDumboOctopuses
    (Right . findFirstSyncronisedFlash)
