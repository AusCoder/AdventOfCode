{-# LANGUAGE OverloadedStrings #-}

module Day5 (day5) where

import Data.Bits (xor)
import Data.Char
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

reduce :: String -> String
reduce =
  let go h [] = reverse h
      go h [x] = go (x : h) []
      go h (x : y : rest) =
        let canReduce x y = (toLower x == toLower y) && xor (isLower x) (isLower y)
         in if canReduce x y
              then uncurry go (push h rest)
              else go (x : h) (y : rest)
      push [] t = ([], t)
      push [x] t = ([], x : t)
      push (x : y : rest) t = (rest, y : x : t)
   in go []

part1 :: T.Text -> Int
part1 = length . reduce . T.unpack

part2 :: T.Text -> Int
part2 t =
  let str = T.unpack t
      uniqLetters = foldr (Set.insert . toLower) Set.empty str
      lenWithoutChar c = length . reduce . filter ((/= c) . toLower) $ str
   in minimum . fmap lenWithoutChar . Set.toList $ uniqLetters

day5 :: IO ()
day5 = do
  content <- T.strip <$> TIO.readFile "input/day5.txt"
  print $ part1 content
  print $ part2 content
