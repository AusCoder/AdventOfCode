module Day2 where

import Common
import Data.Foldable (foldl')
import Text.Parsec

data Instruction
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

parserInstruction :: StringParser Instruction
parserInstruction = do
  f <-
    (Forward <$ string "forward")
      <|> (Down <$ string "down")
      <|> (Up <$ string "up")
  space
  f <$> parserInt

calculateDestination :: [Instruction] -> Int
calculateDestination instrs =
  let go [] (p, d) = p * d
      go (Forward x : xs) (p, d) = go xs (p + x, d)
      go (Up x : xs) (p, d) = go xs (p, d - x)
      go (Down x : xs) (p, d) = go xs (p, d + x)
   in go instrs (0, 0)

day2a =
  runDay
    "input/day2.txt"
    (parserManyLines parserInstruction)
    (Right . calculateDestination)

calculateWithAim :: [Instruction] -> Int
calculateWithAim =
  let foldFn (p, d, a) (Forward x) = (p + x, d + a * x, a)
      foldFn (p, d, a) (Up x) = (p, d, a - x)
      foldFn (p, d, a) (Down x) = (p, d, a + x)
   in (\(p, d, _) -> p * d) . foldl' foldFn (0, 0, 0)

day2b =
  runDay
    "input/day2.txt"
    (parserManyLines parserInstruction)
    (Right . calculateWithAim)
