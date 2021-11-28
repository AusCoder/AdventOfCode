module Day3 where

import Common
import qualified Data.Map.Strict as Map
import Text.Parsec

data Claim = Claim
  { claimId :: Int,
    left :: Int,
    top :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show)

parserClaim :: StringParser Claim
parserClaim = do
  cId <- char '#' >> parserInt
  _ <- space >> char '@' >> space
  l <- parserInt
  t <- char ',' >> parserInt
  w <- char ':' >> space >> parserInt
  h <- char 'x' >> parserInt
  return $ Claim cId l t w h

parserClaims :: StringParser [Claim]
parserClaims = many (parserClaim >>= \x -> x <$ endOfLine)

-- Idea is:
--  sort by x coords
--  for each x coord, calculate the intersection height
intersectionArea :: [Claim] -> Int
intersectionArea = undefined

-- day3a = runDayWithParser "input/day3-test.txt" parserClaims (b -> Either AOCError a)
