module Day12 where

import Common
import Data.Array
import Data.Foldable (foldl')
import Debug.Trace
import Text.Parsec

data Note = Note String Char deriving (Show)

data Problem = Problem String [Note] deriving (Show)

parserProblem :: StringParser Problem
parserProblem = do
  string "initial state: "
  init <- many parserPlant
  endOfLine
  endOfLine
  notes <- parserManyLines parserNote
  return $ Problem init notes

parserNote :: StringParser Note
parserNote = do
  pat <- count 5 parserPlant
  string " => "
  c <- parserPlant
  return $ Note pat c

parserPlant = char '.' <|> char '#'

patToInt :: String -> Int
patToInt = foldl' (\acc c -> if c == '#' then 2 * acc + 1 else 2 * acc) 0

buildNoteArray :: [Note] -> Array Int Char
buildNoteArray notes =
  let assoc = fmap (\(Note pat c) -> (patToInt pat, c)) notes
      addIfMissing i acc = if i `elem` fmap fst acc then acc else (i, '.') : acc
      assoc' = foldr addIfMissing assoc [0 .. 31]
   in array (0, 31) assoc'

notes = [Note "#." '#', Note "##" '#']

data St = St Int String String

stCreate s = St (4 + length s) ('.' : '.' : '.' : '.' : s) (repeat '.')

stTake 0 _ = []
stTake n (St _ [] b) = take n b
stTake n (St m (a : as) b) = a : stTake (n -1) (St (m - 1) as b)

stTail (St m [] b) = St m [] (tail b)
stTail (St m (_ : as) b) = St (m -1) as b

growPlants :: Int -> Problem -> Int
growPlants numGenerations (Problem initialState notes) =
  let noteArr = buildNoteArray notes

      getChar st = noteArr ! patToInt (stTake 5 st)

      trim [] = []
      trim ('.' : xs) = trim xs
      trim xs = xs

      doEnd st@(St sLen s _) =
        if null s then [] else getChar st : doEnd (stTail st)

      goGen st@(St sLen s _)
        | sLen < 5 = reverse . trim . reverse . doEnd $ st
        | otherwise = getChar st : goGen (stTail st)

      calcStartIdx idx ('.' : xs) = calcStartIdx (idx + 1) xs
      calcStartIdx idx _ = idx

      sumPlantIdxs s idx =
        let foldFn (i, c) acc = if c == '#' then acc + i else acc
         in foldr foldFn 0 $ zip [idx ..] s

      go 0 s idx = sumPlantIdxs s idx
      go n s startIdx =
        let s' = goGen . stCreate $ s
            startIdx' = calcStartIdx (startIdx - 2) s'
            next = go (n - 1) (trim s') startIdx'
         in next
   in --  trace
      --     ( "genNum: " ++ show (numGenerations' - n)
      --         ++ " diff "
      --         ++ show (sumPlantIdxs s' startIdx' - sumPlantIdxs s startIdx)
      --     )
      --     next
      go numGenerations initialState 0

numGenerations' = 20

day12a = runDay "input/day12.txt" parserProblem (Right . growPlants numGenerations')

-- Question: how do I build a tri like this?
-- thats a good problem to try and solve in haskell

-- data SearchNode
--   = SearchNode Char (Maybe SearchNode) (Maybe SearchNode)
--   | Leaf Char Char
--   deriving (Show)

-- data RootNode = RootNode SearchNode SearchNode deriving (Show)

-- -- #.#.##  ----> SearchNode '#' (SearchNode '.')

-- buildTree :: [Note] -> Maybe SearchNode
-- buildTree notes =
--   let buildNode [c] r = Leaf c r
--       buildNode (c : cs) r = SearchNode c (buildNode cs r) Nothing

--       go (Note pat r : rest) Nothing =
--         buildNode pat r
--    in go notes Nothing
