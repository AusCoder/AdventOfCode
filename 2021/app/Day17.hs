{-# LANGUAGE TupleSections #-}

module Day17 where

import Common
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec (char, string)

type Point = (Int, Int)

type Target = (Point, Point)

parserTarget = do
  string "target area: x="
  x0 <- parserSignedInt
  string ".."
  x1 <- parserSignedInt
  string ", y="
  y0 <- parserSignedInt
  string ".."
  y1 <- parserSignedInt
  return ((x0, x1), (y0, y1))

vxMinLargeN :: Target -> Int
vxMinLargeN ((x0, _), _) = ceiling (sqrt (2 * fromIntegral x0 + 1 / 4) - 1 / 2)

vxMaxLargeN :: Target -> Int
vxMaxLargeN ((_, x1), _) = floor (sqrt (2 * fromIntegral x1 + 1 / 4) - 1 / 2)

maxY :: Point -> Int
maxY (_, vy) =
  max
    (vy * vy - vy * (vy - 1) `div` 2)
    (vy * (vy + 1) - (vy + 1) * vy `div` 2)

boundNeg x vx =
  (2 * fromIntegral vx + 1) / 2
    - sqrt (((2 * fromIntegral vx + 1) / 2) ^ 2 - 2 * fromIntegral x)

boundPos x vx =
  (2 * fromIntegral vx + 1) / 2
    + sqrt (((2 * fromIntegral vx + 1) / 2) ^ 2 - 2 * fromIntegral x)

velSmallN :: Target -> [Point]
velSmallN target@((x0, x1), (y0, y1)) =
  let uppern vx = floor $ boundNeg x1 vx
      lowern vx = ceiling $ boundNeg x0 vx
   in [vxMaxLargeN target + 1 .. x1] >>= \vx ->
        [lowern vx .. uppern vx] >>= \n ->
          let vymin = ceiling $ fromIntegral y0 / fromIntegral n + (fromIntegral n -1) / 2
              vymax = floor $ fromIntegral y1 / fromIntegral n + (fromIntegral n -1) / 2
           in [ (vx, vy)
                | vy <- [vymin .. vymax],
                  isInTarget target (pos (vx, vy) n)
              ]

velLargeN :: Target -> [Point]
velLargeN target@((x0, x1), (y0, y1)) =
  let uppern vy = floor $ boundPos y0 vy
      lowern vy = ceiling $ boundPos y1 vy
   in [vxMinLargeN target .. vxMaxLargeN target] >>= \vx ->
        [y0 .. 2 - y0] >>= \vy ->
          [lowern vy .. uppern vy] >>= \n ->
            [(vx, vy) | isInTarget target (pos (vx, vy) n)]

isInTarget ((x0, x1), (y0, y1)) (x, y) =
  (x0 <= x) && (x <= x1) && (y0 <= y) && (y <= y1)

f vx n = n * vx - (n * (n - 1)) `div` 2

pos (vx, vy) n = if n <= vx then (f vx n, f vy n) else (f vx vx, f vy n)

target :: Target
target = ((20, 30), (-10, -5))

calcMaxHeight :: Target -> Int
calcMaxHeight target =
  maximum (fmap maxY (velSmallN target)) `max` maximum (fmap maxY (velLargeN target))

day17a =
  runDay
    "input/day17.txt"
    parserTarget
    (Right . calcMaxHeight)

countVels :: Target -> Int
countVels target =
  Set.size $
    foldr
      Set.insert
      (foldr Set.insert Set.empty (velSmallN target))
      (velLargeN target)

day17b =
  runDay
    "input/day17.txt"
    parserTarget
    (Right . countVels)
