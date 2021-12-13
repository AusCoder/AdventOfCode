module Day13 where

import Common
import Data.Foldable (foldl')
import qualified Data.HashSet as HashSet
import Debug.Trace
import Text.Parsec (char, endOfLine, many1, string, try, (<|>))

type TransparentPaper = HashSet.HashSet (Int, Int)

data Fold = FoldAlongX Int | FoldAlongY Int deriving (Show)

data DotsAndFolds = DotsAndFolds TransparentPaper [Fold]
  deriving (Show)

parserDotsAndFolds =
  let parserDot = do
        x <- parserInt
        char ','
        y <- parserInt
        endOfLine
        return (x, y)

      parserDots = foldr HashSet.insert HashSet.empty <$> many1 parserDot

      parserFolds = many1 $ do
        string "fold along "
        cstor <- try (char 'x' >> return FoldAlongX) <|> try (char 'y' >> return FoldAlongY)
        char '='
        f <- cstor <$> parserInt
        endOfLine
        return f
   in do
        dots <- parserDots
        endOfLine
        DotsAndFolds dots <$> parserFolds

-- 14 -> 0
-- 13 -> 1
-- 8 -> 6
performFold :: Fold -> TransparentPaper -> TransparentPaper
performFold (FoldAlongX axisX) dots =
  let foldFn (x, y) acc =
        HashSet.insert (if x < axisX then (x, y) else (2 * axisX - x, y)) acc
   in foldr foldFn HashSet.empty dots
performFold (FoldAlongY axisY) dots =
  let foldFn (x, y) acc =
        HashSet.insert (if y < axisY then (x, y) else (x, 2 * axisY - y)) acc
   in foldr foldFn HashSet.empty dots

calculateDotsAfterFirstFold :: DotsAndFolds -> Either AOCError Int
calculateDotsAfterFirstFold (DotsAndFolds _ []) = Left (AOCCustomError "expected at least one fold")
calculateDotsAfterFirstFold (DotsAndFolds dots (f : _)) =
  return . HashSet.size . performFold f $ dots

day13a =
  runDay
    "input/day13.txt"
    parserDotsAndFolds
    calculateDotsAfterFirstFold

renderThermalCode :: DotsAndFolds -> String
renderThermalCode (DotsAndFolds dots folds) =
  let dots' = foldl' (flip performFold) dots folds

      xs = HashSet.map fst dots'
      ys = HashSet.map snd dots'
      renderRow y =
        map
          (\x -> if (x, y) `HashSet.member` dots' then '#' else '.')
          [minimum xs .. maximum xs]
      renderedGrid =
        unlines $
          map
            renderRow
            [minimum ys .. maximum ys]
   in renderedGrid

day13b =
  runDayIO
    "input/day13.txt"
    parserDotsAndFolds
    (\dafs -> Right <$> putStrLn (renderThermalCode dafs))
