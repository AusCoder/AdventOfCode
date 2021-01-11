{-# LANGUAGE OverloadedStrings #-}

module Day2 (day2) where

import qualified Data.Map.Strict as M
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

letterCounts :: T.Text -> M.Map Char Int
letterCounts =
  let build m (x : xs) =
        let value = maybe 1 (1 +) $ M.lookup x m
         in build (M.insert x value m) xs
      build m _ = m
   in build M.empty . T.unpack

idCharCounts :: T.Text -> (Int, Int)
idCharCounts =
  let foldFn x (count2, count3) = case x of
        2 -> (1, count3)
        3 -> (count2, 1)
        _ -> (count2, count3)
   in foldr foldFn (0, 0) . letterCounts

part1 :: T.Text -> Int
part1 =
  let pairwiseAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
      tupProd (x, y) = x * y
   in tupProd . foldr pairwiseAdd (0, 0) . fmap idCharCounts . T.lines

letterDiffCount :: T.Text -> T.Text -> Int
letterDiffCount t =
  let go acc ((x, y) : rest) = go (if x /= y then acc + 1 else acc) rest
      go acc _ = acc
   in go 0 . T.zip t

pairwiseCombs :: [a] -> [(a, a)]
pairwiseCombs xs = (,) <$> xs <*> xs

-- pairwiseCombs [] = []
-- pairwiseCombs [x] = []
-- pairwiseCombs [x,y] = [(x,y)]
-- pairwiseCombs (x:xs) = pairwiseCombs xs

part2 :: T.Text -> T.Text
part2 = fmap (uncurry letterDiffCount) . pairwiseCombs . T.lines

-- let lines = T.lines

day2 :: IO ()
day2 = do
  content <- TIO.readFile "input/day2.txt"
  -- print $ part1 content
  -- print $ letterDiffCount "fghij" "fguij"
  print $ pairwiseCombs [1, 2, 3]
