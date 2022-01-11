{-# LANGUAGE TupleSections #-}

module Day19 where

import Common
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec (char, endOfLine, many1, optional, string)

type Point = (Int, Int, Int)

type Rotation = Point -> Point

data ScannerData = ScannerData
  { scanDataId :: Int,
    scanDataPoints :: [Point]
  }
  deriving (Show)

ptSubtr (x, y, z) (a, b, c) = (x - a, y - b, z - c)

ptAdd (x, y, z) (a, b, c) = (x + a, y + b, z + c)

parserScannerData = do
  sId <- string "--- scanner " *> parserInt
  string " ---" *> endOfLine
  points <- many1 parserPoint
  return $ ScannerData sId points
  where
    parserPoint =
      parserSignedInt >>= \x ->
        char ',' *> parserSignedInt >>= \y ->
          char ',' *> parserSignedInt >>= \z ->
            (x, y, z) <$ endOfLine

parserScannerDatas = many1 (parserScannerData <* optional endOfLine)

rots2d =
  [ \(x, y, z) -> (x, y, z),
    \(x, y, z) -> (x, z, - y),
    \(x, y, z) -> (x, - y, - z),
    \(x, y, z) -> (x, - z, y)
  ]

rots3d =
  [ \(x, y, z) -> (x, y, z),
    \(x, y, z) -> (- x, - y, z),
    \(x, y, z) -> (y, z, x),
    \(x, y, z) -> (- y, z, - x),
    \(x, y, z) -> (z, x, y),
    \(x, y, z) -> (- z, - x, y)
  ]
    >>= \fn ->
      rots2d >>= \fn' ->
        return (fn . fn')

countBeacons :: [ScannerData] -> Int
countBeacons scanData =
  let scannerPosns = calcScannerPosnsRelScanner0 scanData
      addBeacons (ScannerData _ pts, posnRel0, rotTo0) acc =
        foldr Set.insert acc (ptAdd posnRel0 . rotTo0 <$> pts)
      beaconPosns = foldr addBeacons Set.empty scannerPosns
   in Set.size beaconPosns

calcScannerPosnsRelScanner0 scanData =
  let scanData0 = head . filter ((== 0) . scanDataId) $ scanData

      findConnected _ [] _ = Nothing
      findConnected orig (_ : unknown) [] = findConnected orig unknown orig
      findConnected orig (unknownSd : unknownRest) ((knownSd, knownRel0, knownRot) : knownRest) =
        case findCommonUsingRot (scanDataPoints knownSd) (scanDataPoints unknownSd) of
          Nothing -> findConnected orig (unknownSd : unknownRest) knownRest
          Just (p1, p2, rot) ->
            Just (unknownSd, knownRel0 `ptAdd` knownRot (p1 `ptSubtr` rot p2), knownRot . rot)

      -- find an overlap with existing data, add it to the map, repeat
      go :: HashMap.HashMap Int (ScannerData, Point, Rotation) -> HashMap.HashMap Int (ScannerData, Point, Rotation)
      go relPosns =
        let notSeen = filter (not . (`HashMap.member` relPosns) . scanDataId) scanData
         in if null notSeen
              then relPosns
              else
                let relPosnsL = HashMap.elems relPosns
                    relPosns' = case findConnected relPosnsL notSeen relPosnsL of
                      Nothing -> error "didn't expect this I guess?"
                      Just (sd, p, r) -> HashMap.insert (scanDataId sd) (sd, p, r) relPosns
                 in go relPosns'
   in go (HashMap.singleton 0 (scanData0, (0, 0, 0), id))

day19a =
  runDay
    "input/day19.txt"
    parserScannerDatas
    (Right . countBeacons)

samplePs1 =
  [ (0, 2, 0),
    (4, 1, 0),
    (3, 3, 0)
  ]

samplePs2 =
  [ (-1, -1, 0),
    (-5, 0, 0),
    (-2, 1, 0)
  ]

countCommon points1 points2 =
  length $ filter (`elem` points2) points1

findCommon n rot points1 points2 =
  let points2rot = fmap rot points2
      pairs = zip points2 points2rot

      go [] _ = Nothing
      go (p1 : pts1) [] = go pts1 pairs
      go (p1 : pts1) ((p2, p2rot) : pts2) =
        let rel1 = fmap (`ptSubtr` p1) points1
            rel2 = fmap (`ptSubtr` p2rot) points2rot
            c = countCommon rel1 rel2
         in if c >= n
              then Just (p1, p2, rot)
              else go (p1 : pts1) pts2
   in go points1 pairs

findCommonUsingRot points1 points2 =
  let go [] = Nothing
      go (r : rs) =
        case findCommon 12 r points1 points2 of
          Just ps -> Just ps
          Nothing -> go rs
   in go rots3d

distance (x, y, z) (a, b, c) = abs (x - a) + abs (y - b) + abs (z - c)

maxScannerDistance :: [ScannerData] -> Int
maxScannerDistance scanData =
  let scannerPosns = fmap (\(_, p, _) -> p) . HashMap.elems $ calcScannerPosnsRelScanner0 scanData
   in maximum . fmap (uncurry distance) $ pairs scannerPosns

day19b =
  runDay
    "input/day19.txt"
    parserScannerDatas
    (Right . maxScannerDistance)
