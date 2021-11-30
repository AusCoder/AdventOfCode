module Day10 where

import Common
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec

data Point = Point
  { position :: (Int, Int),
    velocity :: (Int, Int)
  }
  deriving (Show)

parserPoint = do
  string "position=<" >> many space
  x <- parserSignedInt
  char ',' >> many space
  y <- parserSignedInt
  string "> velocity=<" >> many space
  vx <- parserSignedInt
  char ',' >> many space
  vy <- parserSignedInt
  char '>'
  return $ Point (x, y) (vx, vy)

-- wrapper type to make my result printing nice
newtype Grid = Grid {getGrid :: String}

instance Show Grid where
  show = getGrid

findWord :: [Point] -> (Int, Grid)
findWord points =
  let mvPt (Point (x, y) (vx, vy)) = Point (x + vx, y + vy) (vx, vy)

      span pts =
        let xs = fmap (fst . position) $ pts
         in maximum xs - minimum xs

      go acc pts =
        let pts' = fmap mvPt pts
         in if span pts <= span pts'
              then (acc, pts)
              else go (acc + 1) pts'

      (timeTaken, points') = go 0 points
   in (timeTaken, toGrid points')

toGrid :: [Point] -> Grid
toGrid points =
  let positions = fmap position $ points
      xmin = minimum . fmap fst $ positions
      ymin = minimum . fmap snd $ positions
      xmax = maximum . fmap fst $ positions
      ymax = maximum . fmap snd $ positions
      getrow y =
        fmap (\x -> if (x, y) `elem` positions then '#' else '.') [xmin .. xmax]
      rows = fmap getrow [ymin .. ymax]
   in Grid $ unlines rows

day10a = runDay "input/day10.txt" (parserManyLines parserPoint) (Right . snd . findWord)

day10b = runDay "input/day10.txt" (parserManyLines parserPoint) (Right . fst . findWord)
