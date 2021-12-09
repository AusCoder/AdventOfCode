module Day7 where

import Common
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Debug.Trace
import Text.Parsec (char, many)

parserPositions =
  parserInt >>= \x ->
    (x :) <$> many (char ',' >> parserInt)

calculateFuelCostConst :: [Int] -> Int -> Identity Int
calculateFuelCostConst positions x = return $ sum . fmap (abs . (+) (- x)) $ positions

calculateFueldCostTriangular positions x =
  fmap sum . mapM calculateTriangular $ fmap (abs . (+) (- x)) positions

type TriangularCache a = State (Map.Map Int Int) a

calculateTriangular :: Int -> TriangularCache Int
calculateTriangular 0 = return 0
calculateTriangular x
  | x < 0 = error "i shouldnt do this"
  | otherwise = do
    cache <- get
    v <- maybe ((+) x <$> calculateTriangular (x - 1)) return $ Map.lookup x cache
    put $ Map.insert x v cache
    return v

calculateMinFuel :: (Monad m) => ([Int] -> Int -> m Int) -> [Int] -> m Int
calculateMinFuel calcFuel positions =
  let xmin = minimum positions

      go x Nothing =
        calcFuel positions x >>= \fuel ->
          go x (Just (x, fuel))
      go x (Just (prevX, prevFuel)) =
        calcFuel positions x >>= \fuel ->
          if fuel <= prevFuel
            then go (x + 1) (Just (x, fuel))
            else return prevFuel
   in go xmin Nothing

day7a =
  runDay
    "input/day7.txt"
    parserPositions
    (Right . runIdentity . calculateMinFuel calculateFuelCostConst)

day7b =
  runDay
    "input/day7.txt"
    parserPositions
    ( \positions ->
        Right $ evalState (calculateMinFuel calculateFueldCostTriangular positions) Map.empty
    )
