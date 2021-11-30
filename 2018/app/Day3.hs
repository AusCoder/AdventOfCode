{-# LANGUAGE TupleSections #-}

module Day3 where

import Common
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Debug.Trace
import Text.Parsec

data Claim = Claim
  { claimId :: Int,
    left :: Int,
    top :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show)

claimXMin = left

claimYMin = top

claimXMax c = left c + width c

claimYMax c = top c + height c

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
intersectionArea claims =
  let xCoords = sort $ claims >>= \c -> [claimXMin c, claimXMax c]
      containsX x c = x >= claimXMin c && x < claimXMax c
      claimsAtX x = filter (containsX x) claims
      foldFn acc (x, x') =
        acc
          + ( let i = intersectionLength (claimsAtX x)
                  v = (x' - x) * i
               in v
            )
   in foldl foldFn 0 $ zip xCoords (tail xCoords)

intersectionLength :: [Claim] -> Int
intersectionLength claims =
  let pts = sort $ claims >>= \c -> [(claimYMin c, False), (claimYMax c, True)]
      go inCount startInt acc [] = acc
      go inCount startInt acc ((y, isEnd) : rest) =
        let newInCount = if isEnd then inCount - 1 else inCount + 1
         in case (inCount, isEnd) of
              (1, False) -> go newInCount (Just y) acc rest
              (2, True) -> go newInCount Nothing (acc + y - fromMaybe undefined startInt) rest
              _ -> go newInCount startInt acc rest
   in go 0 Nothing 0 pts

day3a = runDay "input/day3.txt" parserClaims (Right . intersectionArea)

claimPoints claim = [claimXMin claim .. claimXMax claim - 1] >>= \x -> map (x,) [claimYMin claim .. claimYMax claim - 1]

nonOverlappingClaim :: [Claim] -> Either AOCError Int
nonOverlappingClaim claims =
  let incrPoint p m = Map.insert p ((+) 1 . fromMaybe 0 . Map.lookup p $ m) m
      insertClaim claim m =
        foldr incrPoint m $ claimPoints claim
      cornerCounts = foldr insertClaim Map.empty claims
      hasNoIntersection claim = all (\p -> fromMaybe 0 (Map.lookup p cornerCounts) == 1) (claimPoints claim)
   in fmap claimId . maybeToErr "no head" . headMay . filter hasNoIntersection $ claims

day3b = runDay "input/day3.txt" parserClaims nonOverlappingClaim
