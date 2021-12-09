module Day6 where

import Common
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace
import Text.Parsec (char, many)

parserFish =
  parserInt >>= \h ->
    (h :) <$> many (char ',' >> parserInt)

countFish :: Int -> [Int] -> Int
countFish n initialAges =
  let initialAges' = foldr (\x acc -> HashMap.insertWith (+) x 1 acc) HashMap.empty initialAges

      go 0 ages = sum $ HashMap.elems ages
      go n ages =
        let ages' =
              foldr
                (\(x, n) acc -> HashMap.insertWith (+) (x - 1) n acc)
                HashMap.empty
                $ HashMap.toList ages

            numReproduced =
              sum
                . HashMap.elems
                . HashMap.filterWithKey (\k _ -> k < 0)
                $ ages'

            ages'' =
              HashMap.insertWith (+) 8 numReproduced $
                HashMap.insertWith (+) 6 numReproduced $
                  HashMap.filterWithKey (\k _ -> k >= 0) ages'
         in go (n - 1) ages''
   in go n initialAges'

day6a = runDay "input/day6.txt" parserFish (Right . countFish 80)

day6b = runDay "input/day6.txt" parserFish (Right . countFish 256)
