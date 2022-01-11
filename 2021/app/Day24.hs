{-# LANGUAGE TupleSections #-}

-- This day I basically worked out what the program does on paper
-- and reverse engineered the only digit combinations that would produce
-- 0 as an answer
module Day24 where

import Common
import Data.Char (digitToInt)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Debug.Trace
import Text.Parsec (char, choice, oneOf, string, try, (<|>))

data Arg = RegArg Char | NumArg Int deriving (Show)

data Instruction
  = Input Char
  | Op Char Char Arg
  deriving (Show)

parserInstruction =
  let pChar = oneOf "xyzw"
      pArg = (RegArg <$> pChar) <|> (NumArg <$> parserSignedInt)
      pInpt = Input <$> (string "inp " *> pChar)
      pType =
        choice
          [ Op '+' <$ try (string "add "),
            Op '*' <$ try (string "mul "),
            Op '/' <$ try (string "div "),
            Op '%' <$ try (string "mod "),
            Op '=' <$ try (string "eql ")
          ]
      pInstr = do
        cstr <- pType
        c <- pChar
        a <- char ' ' *> pArg
        return $ cstr c a
   in pInpt <|> pInstr

parserInstructions = parserManyLines parserInstruction

fn n =
  let ds = fmap digitToInt $ show n
   in (head ds + 7) * 26 + (head (tail ds) + 8)

calcMaxValue :: [Instruction] -> Int
calcMaxValue instructions =
  let -- found numbers by hand, this checks alu output for them
      -- nums = [97919997299495]
      nums = [51619131181131]
      go [] = undefined
      go (n : rest) =
        let z = aluGet 'z' $ runProgram aluInit instructions (digitToInt <$> show n)
         in trace (show (n, z)) $ if z == 0 then n else go rest
   in go nums

-- trace (show $ runProgram aluInit instructions (fmap digitToInt "13579246899999")) undefined

newtype ALU = ALU (Map.Map Char Int) deriving (Show)

aluGet c (ALU vals) = fromMaybe undefined $ Map.lookup c vals

aluSet c v (ALU vals) = ALU $ Map.insert c v vals

aluInit = ALU . Map.fromList $ fmap (,0) "xyzw"

runProgram alu [] _ = alu
runProgram alu (instr : rest) inpt =
  case instr of
    Input c ->
      runProgram (aluSet c (head inpt) alu) rest (tail inpt)
    Op op a1 a2 ->
      let a2' = case a2 of
            NumArg x -> x
            RegArg c -> aluGet c alu
          op' = case op of
            '+' -> (+)
            '*' -> (*)
            '/' -> div
            '%' -> mod
            '=' -> \x y -> if x == y then 1 else 0
            _ -> undefined
          v = aluGet a1 alu `op'` a2'
          alu' = aluSet a1 v alu
          run' = runProgram alu' rest inpt
       in if op == '+' && a1 == 'z' then trace (show alu') run' else run'

day24a = runDay "input/day24.txt" parserInstructions (Right . calcMaxValue)
