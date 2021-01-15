module Day7 where

import Common
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, maybeToList)
import Data.Sort (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Safe
import Text.Parsec as P
import qualified Text.Parsec.Char as PC

type NodeId = Char

data Rel = Rel NodeId NodeId deriving (Show)

relParser :: SimpleParser Rel
relParser =
  let p1 = PC.string "Step " *> P.anyChar
      p2 = PC.string " must be finished before step " *> P.anyChar
   in Rel <$> p1 <*> p2

part1 :: T.Text -> Either AOCError [NodeId]
part1 t = mapM (runSimpleParser relParser) (T.lines t) >>= runPart1

-- How do I represent a graph in haskell?
-- Here I keep 2 maps, 1 representing the edges from a node.
-- The other is the indegree of each node.

buildNeighborsFromNode :: [Rel] -> M.Map NodeId [NodeId]
buildNeighborsFromNode =
  let go m [] = m
      go m (Rel x y : rest) =
        let xRels = y : fromMaybe [] (M.lookup x m)
         in go (M.insert x xRels m) rest
   in go M.empty

-- here is the in degrees
buildInDegrees :: [Rel] -> M.Map NodeId Int
buildInDegrees =
  let go m [] = m
      go m (Rel x y : rest) =
        let xn = fromMaybe 0 $ M.lookup x m
            yn = maybe 1 (+ 1) $ M.lookup y m
         in go (M.insert x xn $ M.insert y yn m) rest
   in go M.empty

runPart1 :: [Rel] -> Either AOCError [NodeId]
runPart1 rels =
  let neighbors = buildNeighborsFromNode rels
      lookupNeighbors n = mayToAOCGenericError ("Expected neighbors of " ++ show n) $ M.lookup n neighbors
      decrNeighbors n indegs = maybe indegs (\v -> M.insert n (v - 1) indegs) $ M.lookup n indegs

      go indegs acc
        | M.size indegs == 0 = Right $ reverse acc
        | M.size indegs == 1 = go M.empty $ (head . M.keys $ indegs) : acc
        | otherwise =
          let indegree0Nodes = sort . fmap fst . filter ((== 0) . snd) . M.toList $ indegs
              delAndDecrNeighbors nodeId =
                let newAcc = (nodeId : acc)
                    newIndegsE = foldr decrNeighbors (M.delete nodeId indegs) <$> lookupNeighbors nodeId
                 in newIndegsE >>= \newIndegs -> go newIndegs newAcc
           in mayToAOCGenericError "No indegree 0 node" (headMay indegree0Nodes)
                >>= delAndDecrNeighbors
   in go (buildInDegrees rels) ""

day7 :: IO ()
day7 = do
  content <- TIO.readFile "input/day7.txt"
  print $ part1 content
