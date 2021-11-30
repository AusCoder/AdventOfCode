module Day7 where

import Common
import Data.Char (ord)
import Data.Foldable (minimumBy)
import qualified Data.HashMap.Internal.Strict as HashMap
import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)
import Data.Ord (comparing)
import Debug.Trace
import Text.Parsec (alphaNum, endOfLine, many, string)

data Relation = Relation Char Char deriving (Show)

parserRelation =
  string "Step " >> alphaNum >>= \x ->
    string " must be finished before step " >> alphaNum >>= \y ->
      string " can begin." >> return (Relation x y)

parserRelations = many (parserRelation >>= \r -> r <$ endOfLine)

getIndegrees :: [Relation] -> HashMap.HashMap Char Int
getIndegrees =
  foldr
    (\(Relation x y) m -> HashMap.insertWith (+) x 0 $ HashMap.insertWith (+) y 1 m)
    HashMap.empty

removeNodeAndDecrIndegrees :: [Relation] -> Char -> HashMap.HashMap Char Int -> HashMap.HashMap Char Int
removeNodeAndDecrIndegrees relations node indegs =
  let relationStartsAt x (Relation y _) = x == y
      indegs' = HashMap.delete node indegs
      indegs'' =
        foldr
          (\(Relation _ y) m -> HashMap.adjust ((-1) +) y m)
          indegs'
          $ filter (relationStartsAt node) relations
   in indegs''

orderRelations :: [Relation] -> Either AOCError String
orderRelations relations =
  let indegrees = getIndegrees relations

      go indegs acc
        | HashMap.null indegs = return $ reverse acc
        | otherwise =
          let indegsZero = sort . fmap fst . HashMap.toList . HashMap.filter ((==) 0) $ indegs
           in headErr indegsZero >>= \node ->
                let acc' = node : acc
                    indegs' = removeNodeAndDecrIndegrees relations node indegs
                 in go indegs' acc'
   in go indegrees ""

day7a = runDay "input/day7.txt" parserRelations orderRelations

calculateRuntime :: Int -> Int -> [Relation] -> Either AOCError Int
calculateRuntime workerCount baseDuration relations =
  let indegrees = getIndegrees relations
      workerIds = [0 .. workerCount -1]

      -- advance time, update workers and done tasks, then continue
      advanceTime workers indegs acc =
        let (timeDelta, _) = minimumBy (comparing fst) workers
            workers' = HashMap.map (\(t, x) -> (t - timeDelta, x)) workers
            workers'' = HashMap.filter ((/=) 0 . fst) workers'
            doneTasks = fmap snd . HashMap.elems . HashMap.filter ((==) 0 . fst) $ workers'
            indegs' = foldr (removeNodeAndDecrIndegrees relations) indegs doneTasks
         in go workers'' indegs' (acc + timeDelta)

      -- assign node to a worker, then continue
      assignNode workers indegs acc node =
        ( maybeToErr "no head - workers"
            . headMay
            . filter (not . (`HashMap.member` workers))
            $ workerIds
        )
          >>= \workerId ->
            let taskDuration = baseDuration + 1 + ord node - ord 'A'
                workers' = HashMap.insert workerId (taskDuration, node) workers
             in go workers' indegs acc

      -- go workers indegrees acc
      go workers indegs acc
        | HashMap.null indegs = return acc
        -- all workers assigned -> advance time
        | all (`HashMap.member` workers) workerIds = advanceTime workers indegs acc
        -- some workers not assigned
        | otherwise =
          let assignedNodes = fmap snd . HashMap.elems $ workers
              indegsZero =
                sort
                  . filter (not . (`elem` assignedNodes))
                  . HashMap.keys
                  . HashMap.filter (0 ==)
                  $ indegs
           in case indegsZero of
                [] -> advanceTime workers indegs acc
                (node : _) -> assignNode workers indegs acc node
   in go HashMap.empty indegrees 0

workerCount' = 5

baseDuration' = 60

day7b = runDay "input/day7.txt" parserRelations (calculateRuntime workerCount' baseDuration')
