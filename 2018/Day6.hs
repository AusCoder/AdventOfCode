module Day6 where

import Common
import Safe
import Data.Maybe (maybeToList, fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Text.Parsec as P
import qualified Text.Parsec.Char as PC

data Point = Point Int Int deriving (Show, Eq, Ord)

getX (Point x _) = x

getY (Point _ y) = y

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

pointParser :: SimpleParser Point
pointParser = Point <$> numberParser <* PC.char ',' <* PC.space <*> numberParser

part1 :: T.Text -> Either AOCError Int
part1 t = mapM (runSimpleParser pointParser) (T.lines t) >>= largestFiniteArea

largestFiniteArea :: [Point] -> Either AOCError Int
largestFiniteArea pts =
  let pairs ys [] = []
      pairs ys (x:xs) = (x, reverse ys ++ xs) : pairs (x:ys) xs

      finiteAreas = pairs [] pts >>= maybeToList . uncurry calculateFiniteArea
  in maybe (Left $ AOCGenericError "No finite areas") Right $ maximumMay finiteAreas

calculateFiniteArea :: Point -> [Point] -> Maybe Int
calculateFiniteArea center otherCenters =
  let minX = minimum . fmap getX $ center : otherCenters
      minY = minimum . fmap getY $ center : otherCenters
      maxX = maximum . fmap getX $ center : otherCenters
      maxY = maximum . fmap getY $ center : otherCenters

      isValidPt pt = getX pt >= minX && getX pt <= maxX && getY pt >= minY && getY pt <= maxY

      go seenPoints area [] = Just area
      -- Search around the neighbors of p
      go seenPoints area (p : rest)
        | not (isValidPt p) = Nothing  -- infinite area
        | Set.member p seenPoints = go seenPoints area rest
        | otherwise =
          let isClosestToCenter = all (\q -> manhattanDistance p center < manhattanDistance p q) otherCenters
              newSeenPoints = Set.insert p seenPoints
              newArea = if isClosestToCenter then area + 1 else area
              ns = if isClosestToCenter then neighbors p else []
              newRest = foldr (:) rest . filter (not . flip elem seenPoints) $ ns
          in go newSeenPoints newArea newRest
   in go Set.empty 0 [center]

neighbors :: Point -> [Point]
neighbors (Point x y) =
  [ Point (x - 1) y,
    Point (x + 1) y,
    Point x (y - 1),
    Point x (y + 1)
  ]

part2 :: T.Text -> Either AOCError Int
part2 t = closeRegionSize 10000 <$> mapM (runSimpleParser pointParser) (T.lines t)

closeRegionSize :: Int -> [Point] -> Int
closeRegionSize maxDist pts =
  let minX = minimum . fmap getX $ pts
      minY = minimum . fmap getY $ pts
      maxX = maximum . fmap getX $ pts
      maxY = maximum . fmap getY $ pts

      allPts = do
        x <- [minX..maxX]
        Point x <$> [minY..maxY]

      isClose p = sum (manhattanDistance p <$> pts) < maxDist

   in length . filter isClose $ allPts

day6 :: IO ()
day6 = do
  content <- TIO.readFile "input/day6.txt"
  print $ part1 content
  print $ part2 content
