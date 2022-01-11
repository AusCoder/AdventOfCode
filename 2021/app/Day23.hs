-- you can do a priority queue for dijkstra with:
-- {n: [nodes with cost n]}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Day23 where

import Common
import Control.Monad.State
import Data.Foldable (minimumBy)
import Data.HashMap.Strict ((!?))
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Sort (sortOn)
import Debug.Trace
import GHC.Generics (Generic)
import Text.Parsec (char, count, endOfLine, many, many1, oneOf, string)

-- Solved by hand
day22a = 2000 + 300 + 3000 + 4000 + 5 + 5 + 60 + 500 + 600 + 50 + 3 + 3

xIdxs = [2 * x | x <- [1 .. 4]]

data Amphiod = Amphiod Char Int deriving (Show, Eq, Ord, Generic)

amphiodType (Amphiod c _) = c

getColForType c = head . fmap snd . filter ((== c) . fst) $ zip "ABCD" xIdxs

instance Hashable Amphiod

parserPuzzle = do
  string "#############" >> endOfLine >> char '#'
  many1 (char '.') >> char '#' >> endOfLine
  rows <- count 2 row
  let rows' = head rows : (extraRows ++ tail rows)
  let posns =
        zip [1 ..] rows' >>= \(y, row) ->
          fmap (\(x, c) -> ((x, y), c)) row
  return . fmap (\(i, (p, c)) -> (p, Amphiod c i)) $ zip [0 ..] posns
  where
    row = count 3 (oneOf " #") *> amphs <* (many (oneOf " #") >> endOfLine)
    amphs = zip xIdxs <$> count 4 (amph <* char '#')
    amph = oneOf "ABCD"

extraRows = [zip xIdxs "DCBA", zip xIdxs "DBAC"]

-- extraRows = []

colHeight = 2 + length extraRows

edges =
  let edges =
        [ ((10, 0), [(9, 0)]),
          ((0, 0), [(1, 0)])
        ]
          ++ fmap
            ( \x ->
                ( (x, 0),
                  [(x - 1, 0), (x + 1, 0)]
                    ++ [(x, 1) | x `elem` xIdxs]
                )
            )
            [1 .. 9]
          ++ ( xIdxs >>= \x ->
                 fmap
                   (\y -> ((x, y), (x, y -1) : [(x, y + 1) | y /= colHeight]))
                   [1 .. colHeight]
             )
   in foldr (uncurry Map.insert) Map.empty edges

calcDestinations moveCounts amph@(Amphiod c _) =
  let dstX = getColForType c
      extras =
        if fromMaybe 0 (HashMap.lookup amph moveCounts) > 0
          then []
          else [(0, 0), (10, 0)] ++ [(2 * x + 1, 0) | x <- [0 .. 4]]
   in fmap (dstX,) [1 .. colHeight] ++ extras

isAmphiodTypeSolved positions c =
  let posns = Set.fromList . fmap snd . filter ((== c) . amphiodType . fst) . HashMap.toList $ positions
   in posns == Set.fromList (fmap (getColForType c,) [1 .. colHeight])

isInPartialColumn positions amph@(Amphiod c _) =
  let (curX, curY) = fromMaybe undefined $ HashMap.lookup amph positions
      posns = fmap snd . filter ((== c) . amphiodType . fst) . HashMap.toList $ positions
      dstX = getColForType c
      pts = fmap (dstX,) [curY .. colHeight]
   in (curX == dstX) && all (`elem` posns) pts

getCostPerMove c =
  case c of
    'A' -> 1
    'B' -> 10
    'C' -> 100
    'D' -> 1000

calcRoomDsts positions amph@(Amphiod c _) =
  let pts = fmap (getColForType c,) $ reverse [1 .. colHeight]
      posns = HashMap.toList positions

      go [] = []
      go (x : xs) =
        case headMay (fst <$> filter ((== x) . snd) posns) of
          Nothing -> [x]
          Just (Amphiod c' _) ->
            if c == c' then go xs else []
   in go pts

calcPossibleMoves :: Positions -> MoveCounts -> Amphiod -> [((Int, Int), Int)]
calcPossibleMoves positions moveCounts amph@(Amphiod c _)
  | fromMaybe 0 (HashMap.lookup amph moveCounts) >= 2 = []
  | isInPartialColumn positions amph = []
  | otherwise =
    let curPos = fromMaybe undefined $ HashMap.lookup amph positions
        costPerMove = getCostPerMove c

        hallwayDsts =
          if fromMaybe 0 (HashMap.lookup amph moveCounts) > 0
            then []
            else [(0, 0), (10, 0)] ++ [(2 * x + 1, 0) | x <- [0 .. 4]]
        roomDsts = calcRoomDsts positions amph
        dsts = hallwayDsts ++ roomDsts

        go [] _ acc = acc
        go ((x, cost) : rest) seen acc
          | Set.member x seen = go rest seen acc
          | otherwise =
            let acc' = if (x /= curPos) && (x `elem` dsts) then (x, cost) : acc else acc
                seen' = Set.insert x seen
                stack =
                  foldr (\p acc -> (p, cost + costPerMove) : acc) rest
                    . filter (not . (`elem` HashMap.elems positions))
                    . filter (not . (`Set.member` seen'))
                    . fromMaybe undefined
                    . Map.lookup x
                    $ edges
             in go stack seen' acc'

        moves = go [(curPos, 0)] Set.empty []
     in moves

isSolved positions = all (isAmphiodTypeSolved positions) "ABCD"

type Positions = HashMap.HashMap Amphiod (Int, Int)

type MoveCounts = HashMap.HashMap Amphiod Int

type PositionWithCost = HashMap.HashMap (Positions, MoveCounts) Int

-- need to do a graph search across the states

search n known frontier =
  case fromMaybe [] (frontier !? n) of
    [] -> search (n + 1) known frontier
    (curState@(positions, moveCounts) : rest) ->
      let frontier' = HashMap.insert n rest frontier
       in if curState `HashMap.member` known
            then search n known frontier'
            else
              if isSolved positions
                then n
                else
                  let known' = HashMap.insert curState n known
                      amphs = HashMap.keys positions
                      moves =
                        amphs
                          >>= ( \amph ->
                                  fmap
                                    (amph,)
                                    (calcPossibleMoves positions moveCounts amph)
                              )

                      frontier'' =
                        foldr
                          (\(cost, newState) acc -> HashMap.insertWith (++) (n + cost) [newState] acc)
                          frontier'
                          . filter
                            (\(_, newState) -> not $ newState `HashMap.member` known')
                          . fmap
                            ( \(amph, (posn, cost)) ->
                                (cost, (HashMap.insert amph posn positions, HashMap.insertWith (+) amph 1 moveCounts))
                            )
                          $ moves
                   in search n known' frontier''

calcMinMoves :: [((Int, Int), Amphiod)] -> Int
calcMinMoves puzzle =
  let initialPositions = foldr (uncurry $ flip HashMap.insert) HashMap.empty puzzle
      initialMoveCounts = HashMap.empty
   in search 0 HashMap.empty (HashMap.singleton 0 [(initialPositions, initialMoveCounts)])

calcMinMoves' :: [((Int, Int), Amphiod)] -> Int
calcMinMoves' puzzle =
  let --     go positions [] acc = trace ("isSolved: " ++ show (isSolved positions) ++ " positions: " ++ show positions) acc
      --     go positions ((amph, (posn, cost)) : rest) acc =
      --       go (HashMap.insert amph posn positions) rest (acc + cost)
      --  in Just $ go initialPositions sampleMoves 0
      initialPositions =
        HashMap.fromList
          [ (Amphiod 'C' 6, (6, 2)),
            (Amphiod 'D' 3, (8, 1)),
            (Amphiod 'D' 5, (5, 0)),
            (Amphiod 'C' 1, (6, 1)),
            (Amphiod 'B' 2, (4, 2)),
            (Amphiod 'B' 0, (4, 1)),
            (Amphiod 'A' 7, (8, 2)),
            (Amphiod 'A' 4, (2, 2))
          ]
      initialMoveCounts =
        HashMap.fromList
          [ (Amphiod 'D' 5, 1)
          ]
   in search 0 HashMap.empty (HashMap.singleton 0 [(initialPositions, initialMoveCounts)])

sampleMoves =
  [ (Amphiod 'B' 2, ((3, 0), 40)),
    (Amphiod 'C' 1, ((6, 1), 400)),
    (Amphiod 'D' 5, ((5, 0), 3000)),
    (Amphiod 'B' 2, ((4, 2), 30)),
    (Amphiod 'B' 0, ((4, 1), 40)),
    (Amphiod 'D' 3, ((7, 0), 2000)),
    (Amphiod 'A' 7, ((9, 0), 3)),
    (Amphiod 'D' 3, ((8, 2), 3000)),
    (Amphiod 'D' 5, ((8, 1), 4000)),
    (Amphiod 'A' 7, ((2, 1), 8))
  ]

day23b =
  runDay "input/day23.txt" parserPuzzle (Right . calcMinMoves)
