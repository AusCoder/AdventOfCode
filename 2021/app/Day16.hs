module Day16 where

import Common
import Debug.Trace
import Numeric (readHex)
import Text.Parsec
import Text.Printf (printf)

hexToBinC :: Char -> Either AOCError String
hexToBinC c =
  case readHex [c] of
    (n, _) : _ -> Right $ printf "%04b" (n :: Int)
    _ -> Left $ AOCCustomError ("cant turn into hex: " ++ [c])

hexToBin :: String -> Either AOCError String
hexToBin = fmap concat . mapM hexToBinC

data OpType = OpSum | OpProd | OpMin | OpMax | OpGT | OpLT | OpEQ deriving (Show)

data PacketBody = Value Int | Operator OpType [Packet] deriving (Show)

data Packet = Packet Int PacketBody deriving (Show)

parserRunOn p = either (unexpected . show) return . runStringParser p

parserBin :: StringParser String
parserBin = many1 alphaNum >>= either (unexpected . show) return . hexToBin

binDigit = char '0' <|> char '1'

parserValue :: StringParser PacketBody
parserValue =
  let val1 = char '1' *> count 4 binDigit
      val0 = char '0' *> count 4 binDigit
   in do
        bs <- many val1
        b <- val0
        return . Value . binToInt . concat $ bs ++ [b]

parserOperator :: Int -> StringParser PacketBody
parserOperator t =
  let parserOp = case t of
        0 -> return OpSum
        1 -> return OpProd
        2 -> return OpMin
        3 -> return OpMax
        5 -> return OpGT
        6 -> return OpLT
        7 -> return OpEQ
        _ -> unexpected ("unknown type " ++ show t)

      pkts15 = do
        l <- binToInt <$> count 15 binDigit
        sub <- count l binDigit
        parserRunOn (many1 parserPacket) sub

      pkts11 = do
        n <- binToInt <$> count 11 binDigit
        count n parserPacket
   in do
        c <- binDigit
        pkts <- if c == '0' then pkts15 else pkts11
        op <- parserOp
        return $ Operator op pkts

parserPacket :: StringParser Packet
parserPacket = do
  v <- binToInt <$> count 3 binDigit
  t <- binToInt <$> count 3 binDigit
  body <- if t == 4 then parserValue else parserOperator t
  return $ Packet v body

sumVersions :: Packet -> Int
sumVersions (Packet v (Value _)) = v
sumVersions (Packet v (Operator _ pkts)) = v + sum (map sumVersions pkts)

day16a =
  runDay
    "input/day16.txt"
    (parserBin >>= parserRunOn parserPacket)
    (Right . sumVersions)

eval :: Packet -> Int
eval (Packet _ (Value v)) = v
eval (Packet _ (Operator op pkts)) =
  let vals = fmap eval pkts
   in case op of
        OpSum -> sum vals
        OpProd -> product vals
        OpMin -> minimum vals
        OpMax -> maximum vals
        OpGT -> if head vals > head (tail vals) then 1 else 0
        OpLT -> if head vals < head (tail vals) then 1 else 0
        OpEQ -> if head vals == head (tail vals) then 1 else 0

day16b =
  runDay
    "input/day16.txt"
    (parserBin >>= parserRunOn parserPacket)
    (Right . eval)
