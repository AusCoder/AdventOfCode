module Day6 where

import Common
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Text.Parsec as P
import qualified Text.Parsec.Char as PC

data Point = Point Int Int deriving (Show, Eq, Ord)

manhattanDistance :: Point -> Point -> Int
manhattanDistance = undefined

pointParser :: SimpleParser Point
pointParser = Point <$> numberParser <* PC.char ',' <* PC.space <*> numberParser

part1 :: T.Text -> Either AOCError (Maybe Int)
part1 = fmap largestFiniteArea . mapM (runSimpleParser pointParser) . T.lines

largestFiniteArea :: [Point] -> Maybe Int
largestFiniteArea pts = calculateFiniteArea (head pts) (tail pts)

-- Do a search around this point
calculateFiniteArea :: Point -> [Point] -> Maybe Int
calculateFiniteArea center otherCenters =
  -- go seenPoints area pointsToExplore
  let go seenPoints area [] = Just area
      go seenPoints area (p : rest) =
        let isClostToCenter = all (\q -> manhattanDistance p center < manhattanDistance p q) otherCenters
            newSeenPoints = Set.insert p seenPoints
            newArea = if isClostToCenter then area + 1 else area
            newRest = foldr (:) rest . filter (not . flip elem seenPoints) . neighbors $ p
         in trace (show newRest) $ go newSeenPoints newArea newRest
   in go Set.empty 0 [center]

neighbors :: Point -> [Point]
neighbors (Point x y) =
  [ Point (x - 1) y,
    Point (x + 1) y,
    Point x (y - 1),
    Point x (y + 1)
  ]

day6 :: IO ()
day6 = do
  content <- TIO.readFile "input/day6.txt"
  print $ part1 content
