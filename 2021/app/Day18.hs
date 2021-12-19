module Day18 where

import Common
import Debug.Trace
import Text.Parsec (char, try, (<|>))

data SNum = Value Int | Pair SNum SNum deriving (Show)

parserSNum = do
  char '['
  l <- sub
  char ','
  r <- sub
  char ']'
  return $ Pair l r
  where
    sub = try (Value <$> parserInt) <|> parserSNum

add = Pair

addToRight y (Value x) = Value (x + y)
addToRight y (Pair l r) = Pair (addToRight y l) r

addToLeft y (Value x) = Value (x + y)
addToLeft y (Pair l r) = Pair l (addToLeft y r)

data AddInstr = NoAdd | AddLeft Int | AddRight Int deriving (Show)

explode _ x@(Value _) = (False, NoAdd, x)
explode 4 (Pair l@(Pair (Value x) (Value y)) r) =
  (True, AddLeft x, Pair (Value 0) (addToRight y r))
explode 4 (Pair l r@(Pair (Value x) (Value y))) =
  (True, AddRight y, Pair (addToLeft x l) (Value 0))
explode n (Pair l r) =
  case explode (n + 1) l of
    (didChange, AddLeft x, l') -> (didChange, AddLeft x, Pair l' r)
    (didChange, AddRight x, l') -> (didChange, NoAdd, Pair l' (addToRight x r))
    (didChange, NoAdd, l') ->
      if didChange
        then (didChange, NoAdd, Pair l' r)
        else case explode (n + 1) r of
          (didChange, AddLeft x, r') -> (didChange, NoAdd, Pair (addToLeft x l) r')
          (didChange, AddRight x, r') -> (didChange, AddRight x, Pair l r')
          (didChange, NoAdd, r') -> (didChange, NoAdd, Pair l r')

split (Value x) =
  if x >= 10
    then
      let l = x `div` 2
       in (True, Pair (Value l) (Value $ x - l))
    else (False, Value x)
split (Pair l r) =
  let (didChange, l') = split l
   in if didChange
        then (didChange, Pair l' r)
        else
          let (didChange', r') = split r
           in (didChange', Pair l r')

reduce snum =
  let (didChange, _, snum') = explode 1 snum
   in if didChange
        then reduce snum'
        else
          let (didChange', snum'') = split snum
           in if didChange' then reduce snum'' else snum

magnitude (Value x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

ex1 = Pair (Pair (Pair (Pair (Pair (Value 9) (Value 8)) (Value 1)) (Value 2)) (Value 3)) (Value 4)

ex2 = Pair (Value 7) (Pair (Value 6) (Pair (Value 5) (Pair (Value 4) (Pair (Value 3) (Value 2)))))

calcMagnitude :: [SNum] -> Either AOCError Int
calcMagnitude [] = Left $ AOCCustomError "need at least one num"
calcMagnitude (h : snums) =
  let result = foldl (\acc sn -> reduce $ acc `add` sn) h snums
   in Right . magnitude $ result

day18a =
  runDay
    "input/day18.txt"
    (parserManyLines parserSNum)
    calcMagnitude

largestMagnitude :: [SNum] -> Int
largestMagnitude snums =
  maximum . fmap magnitude $
    pairs snums >>= \(sn, sn') ->
      [reduce (sn `add` sn'), reduce (sn' `add` sn)]

day18b =
  runDay
    "input/day18.txt"
    (parserManyLines parserSNum)
    (Right . largestMagnitude)
