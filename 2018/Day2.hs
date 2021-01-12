{-# LANGUAGE OverloadedStrings #-}

module Day2 (day2) where

import Common (pairwiseCombs)
import qualified Data.Map.Strict as M
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Safe (headMay)

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

part2 :: T.Text -> Either String T.Text
part2 =
  let commonLetters :: T.Text -> T.Text -> T.Text
      commonLetters s = T.pack . fmap fst . filter (uncurry (==)) . T.zip s
      getCommonLetters = maybe (Left "Couldn't find pair with diff of 1") (Right . uncurry commonLetters)
   in getCommonLetters . headMay . filter ((==) 1 . uncurry letterDiffCount) . pairwiseCombs . T.lines

day2 :: IO ()
day2 = do
  content <- TIO.readFile "input/day2.txt"
  print $ part1 content
  either print TIO.putStrLn $ part2 content
