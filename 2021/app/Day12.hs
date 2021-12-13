{-# LANGUAGE DeriveGeneric #-}

module Day12 where

import Common
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Debug.Trace
import GHC.Generics (Generic)
import Text.Parsec (char, choice, lower, many1, string, try, upper)

data Cave
  = Start
  | End
  | SmallCave String
  | LargeCave String
  deriving (Show, Eq, Ord, Generic)

instance Hashable Cave

type CaveSystem = HashMap.HashMap Cave [Cave]

isLargeCave (LargeCave _) = True
isLargeCave _ = False

isSmallCave (SmallCave _) = True
isSmallCave _ = False

parserCaves :: StringParser CaveSystem
parserCaves =
  let parserCave =
        choice
          [ try (string "start") >> return Start,
            try (string "end") >> return End,
            try (many1 upper) >>= \s ->
              return $
                LargeCave
                  s,
            try (many1 lower)
              >>= \s -> return $ SmallCave s
          ]

      parserConnectedCaves = do
        c1 <- parserCave
        char '-'
        c2 <- parserCave
        return (c1, c2)
   in parserManyLines parserConnectedCaves
        <&> foldr
          ( \(c1, c2) acc ->
              HashMap.insertWith (++) c1 [c2] $
                HashMap.insertWith (++) c2 [c1] acc
          )
          HashMap.empty

countPaths :: CaveSystem -> Either AOCError Int
countPaths caveSystem =
  let go _ End = return 1
      go seenCaves curCave =
        -- for each adjacent cave, if we haven't been there
        --   go there and add the result
        let seenCaves' = if not (isLargeCave curCave) then Set.insert curCave seenCaves else seenCaves
         in do
              neighbors <- maybeToErr "expected to find cave" (HashMap.lookup curCave caveSystem)
              let neighborsToCheck = filter (not . (`Set.member` seenCaves')) neighbors
              sum <$> mapM (go seenCaves') neighborsToCheck
   in go Set.empty Start

day12a =
  runDay
    "input/day12.txt"
    parserCaves
    countPaths

countPathsWithSingleRepeat :: CaveSystem -> Either AOCError Int
countPathsWithSingleRepeat caveSystem =
  let go _ _ End = return 1
      go seenCaves hasUsedRepeat curCave
        | (curCave == Start) && Set.size seenCaves > 0 = return 0
        | otherwise =
          -- for each adjacent cave, if we haven't been there
          --   go there and add the result
          let seenCaves' = if isSmallCave curCave then Set.insert curCave seenCaves else seenCaves
           in do
                neighbors <- maybeToErr "expected to find cave" (HashMap.lookup curCave caveSystem)
                let neighborsToCheck =
                      filter (not . (`Set.member` seenCaves'))
                        . filter (/= Start)
                        $ neighbors
                pathsFromHere <- sum <$> mapM (go seenCaves' hasUsedRepeat) neighborsToCheck

                -- if not hasUsedRepeat, for each neighboring seen small cave, go there with hasUsedRepeat True
                let seenNeighbors = filter (`Set.member` seenCaves') neighbors
                extraPathsFromRepeat <-
                  if hasUsedRepeat
                    then return 0
                    else sum <$> mapM (go seenCaves' True) seenNeighbors

                return $ pathsFromHere + extraPathsFromRepeat
   in go Set.empty False Start

day12b =
  runDay
    "input/day12.txt"
    parserCaves
    countPathsWithSingleRepeat
