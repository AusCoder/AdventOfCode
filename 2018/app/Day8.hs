module Day8 where

import Common
import Text.Parsec

data Node = Node
  { headerNodeCount :: Int,
    headerMetadataCount :: Int,
    children :: [Node],
    metadata :: [Int]
  }
  deriving (Show)

parserNode :: StringParser Node
parserNode = do
  nodeCount <- many space >> parserInt
  metadataCount <- many space >> parserInt
  ns <- count nodeCount (many space >> parserNode)
  ms <- count metadataCount (many space >> parserInt)
  return $ Node nodeCount metadataCount ns ms

sumMetadata node = sum (metadata node) + sum (fmap sumMetadata (children node))

day8a = runDay "input/day8.txt" parserNode (Right . sumMetadata)

nodeValue node =
  let childValue node childIdx =
        if childIdx <= 0 || childIdx > length (children node)
          then 0
          else nodeValue $ children node !! (childIdx - 1)
   in case children node of
        [] -> sumMetadata node
        _ -> sum . fmap (childValue node) $ metadata node

day8b = runDay "input/day8.txt" parserNode (Right . nodeValue)
