module Day21 where

import Common
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Text.Parsec (endOfLine, string)

parserPosns = do
  p1 <- string "Player 1 starting position: " *> parserInt <* endOfLine
  p2 <- string "Player 2 starting position: " *> parserInt <* endOfLine
  return (p1, p2)

move posn n = ((posn - 1 + n) `mod` 10) + 1

rollDeterministic n v =
  let go 0 acc = acc
      go n (vs, v) =
        go (n -1) (v : vs, (v `mod` 100) + 1)
   in go n ([], v)

calcDeterministic (position1, position2) =
  let go diceValue rollCount (p1, p2) (s1, s2)
        | s1 >= 1000 = s2 * rollCount
        | s2 >= 1000 = s1 * rollCount
        | otherwise =
          let (vals, diceValue') = rollDeterministic 3 diceValue
              p1' = move p1 (sum vals)
           in go diceValue' (rollCount + 3) (p2, p1') (s2, s1 + p1')
   in go 1 0 (position1, position2) (0, 0)

day21a =
  runDay "input/day21.txt" parserPosns (Right . calcDeterministic)

diracRolls = sum <$> cross 3 [1 .. 3]

type Pair = (Integer, Integer)

calcDirac (position1, position2) =
  let doMove p s n =
        let p' = move p n
         in (p', s + p')

      -- 5 (7907,2728)
      play :: Pair -> Pair -> State (Map.Map (Pair, Pair) Pair) Pair
      play (p1, p2) (s1, s2)
        | s1 >= 21 = return (1, 0)
        | s2 >= 21 = return (0, 1)
        | otherwise =
          let key = ((p1, p2), (s1, s2))
           in get >>= \cache ->
                case Map.lookup key cache of
                  Just result -> return result
                  Nothing ->
                    let posns = fmap (doMove p1 s1) diracRolls
                     in mapM (\(p1', s1') -> play (p2, p1') (s2, s1')) posns >>= \counts ->
                          let result = (sum (snd <$> counts), sum (fst <$> counts))
                           in result <$ put (Map.insert key result cache)
   in flip evalState Map.empty $ play (toInteger position1, toInteger position2) (0, 0)

day21b =
  runDay "input/day21.txt" parserPosns (Right . calcDirac)
