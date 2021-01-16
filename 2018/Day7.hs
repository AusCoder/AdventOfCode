{-# LANGUAGE TupleSections #-}

module Day7 where

import Common
import Data.Char (ord)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing, maybeToList)
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

takeFromMapValue :: (Ord k, Num a) => k -> a -> M.Map k a -> M.Map k a
takeFromMapValue k d indegs = maybe indegs (\v -> M.insert k (v - d) indegs) $ M.lookup k indegs

decrMapValue :: (Ord k) => k -> M.Map k Int -> M.Map k Int
decrMapValue = flip takeFromMapValue 1

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
      lookupNeighbors n = fromMaybe [] $ M.lookup n neighbors
      go indegs acc
        | M.size indegs == 0 = Right $ reverse acc
        | otherwise =
          let indegree0Nodes = sort . fmap fst . filter ((== 0) . snd) . M.toList $ indegs
              delAndDecrNeighbors nodeId =
                let newAcc = (nodeId : acc)
                    newIndegs = foldr decrMapValue (M.delete nodeId indegs) $ lookupNeighbors nodeId
                 in go newIndegs newAcc
           in mayToAOCGenericError "No indegree 0 node" (headMay indegree0Nodes)
                >>= delAndDecrNeighbors
   in go (buildInDegrees rels) ""

part2 :: T.Text -> Either AOCError Int
part2 t = mapM (runSimpleParser relParser) (T.lines t) >>= runPart2

type WorkerId = Int

runPart2 :: [Rel] -> Either AOCError Int
runPart2 rels =
  let workerIds = [1 .. 5]
      neighbors = buildNeighborsFromNode rels
      lookupNeighbors n = fromMaybe [] $ M.lookup n neighbors
      timeForNode nodeId = ord nodeId - ord 'A' + 1 + 60

      go :: M.Map NodeId Int -> M.Map WorkerId (Maybe (Char, Int)) -> Int -> Either AOCError Int
      go indegs workers acc
        | M.size indegs == 0 = Right acc
        | otherwise =
          let -- Assign indegree 0 nodes to available workers
              indegree0Nodes = sort . fmap fst . filter ((== 0) . snd) . M.toList $ indegs
              emptyWorkers = fmap fst . filter (isNothing . snd) . M.toList $ workers
              assignedWorkersToNodes = zip emptyWorkers indegree0Nodes
              filledWorkers = foldr (\(w, n) -> M.insert w (Just (n, timeForNode n))) workers assignedWorkersToNodes
              -- rm assigned ingree 0 nodes
              newIndegs' = foldr M.delete indegs . fmap snd $ assignedWorkersToNodes
              -- progress time by min, add to acc
              minTime = minimum $ M.elems filledWorkers >>= fmap snd . maybeToList
              newAcc = acc + minTime
              -- decr indegree of neighbors of completed nodes
              completedNodes = fmap fst . filter ((<= 0) . subtract minTime . snd) $ M.elems filledWorkers >>= maybeToList
              newIndegs = foldr decrMapValue newIndegs' $ completedNodes >>= lookupNeighbors
              -- rm time from workers
              progressWorker k v =
                let decTime c t = if t - minTime <= 0 then Nothing else Just (c, t - minTime)
                 in (k, v >>= uncurry decTime)
              newWorkers :: M.Map WorkerId (Maybe (Char, Int))
              newWorkers = M.fromList . fmap (uncurry progressWorker) . M.toList $ filledWorkers
           in go newIndegs newWorkers newAcc
   in go (buildInDegrees rels) (M.fromList . fmap (,Nothing) $ workerIds) 0

day7 :: IO ()
day7 = do
  content <- TIO.readFile "input/day7.txt"
  print $ part1 content
  print $ part2 content
