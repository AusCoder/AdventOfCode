{-# LANGUAGE OverloadedStrings #-}

module Day1 (day1) where

import Control.Monad
import Data.Char (digitToInt)
import Data.Either (Either (..), either)
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity)
import Data.Maybe (maybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec as P
import qualified Text.Parsec.Char as PC

type Parser a = P.ParsecT T.Text () Identity a

valueParser :: Parser Int
valueParser =
  let numberFromDigits = foldl' (\acc n -> acc * 10 + n) 0
      numP = numberFromDigits . fmap digitToInt <$> P.many PC.digit
   in PC.anyChar >>= \c -> case c of
        '+' -> numP
        '-' -> fmap ((-1) *) numP
        _ -> P.unexpected ("char: " ++ [c])

parseNumbers :: T.Text -> Either String [Int]
parseNumbers = either (Left . show) Right . mapM (P.parse valueParser "") . T.lines

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Either String Int
part2 nums =
  let go s (x : xs) = if Set.member x s then Right x else go (Set.insert x s) xs
      go _ [] = Left "Empty list"
   in go Set.empty . freqSeq . cycle $ nums

freqSeq :: [Int] -> [Int]
freqSeq =
  let go freq (x : xs) = let newFreq = freq + x in newFreq : go newFreq xs
      go _ [] = []
   in go 0

day1 :: IO ()
day1 = do
  content <- TIO.readFile "input/day1.txt"
  let nums = parseNumbers content
  either print (print . part1) nums
  either print print $ nums >>= part2
