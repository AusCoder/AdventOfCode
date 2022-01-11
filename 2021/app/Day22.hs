module Day22 where

import Common
import Data.Foldable (foldl')
import Data.Maybe (isJust, isNothing)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Sort (sort, sortOn)
import Debug.Trace
import Text.Parsec (char, string, try, (<|>))

type Pair = (Int, Int)

type Cube = ((Int, Int, Int), (Int, Int, Int))

data Instruction = Instruction Bool Cube deriving (Show)

parserInstruction = do
  isOn <- (True <$ try (string "on")) <|> (False <$ string "off")
  xmin <- string " x=" *> parserSignedInt
  xmax <- string ".." *> parserSignedInt
  ymin <- string ",y=" *> parserSignedInt
  ymax <- string ".." *> parserSignedInt
  zmin <- string ",z=" *> parserSignedInt
  zmax <- string ".." *> parserSignedInt
  return $ Instruction isOn ((xmin, ymin, zmin), (xmax + 1, ymax + 1, zmax + 1))

calculateDisjointIntervals pairs =
  let endPts =
        sort . Set.toList . foldr Set.insert Set.empty $
          pairs >>= \(x0, x1) -> [x0, x1]
   in zip endPts (tail endPts)

intersect2d ((x0, y0), (x1, y1)) ((a0, b0), (a1, b1)) =
  let b@((x, y), (x', y')) = ((max x0 a0, max y0 b0), (min x1 a1, min y1 b1))
   in if x < x' && y < y' then Just b else Nothing

intersect3d ((x0, y0, z0), (x1, y1, z1)) ((a0, b0, c0), (a1, b1, c1)) =
  let c@((x, y, z), (x', y', z')) =
        ((max x0 a0, max y0 b0, max z0 c0), (min x1 a1, min y1 b1, min z1 c1))
   in if x < x' && y < y' && z < z' then Just c else Nothing

calcDisjointRects r0@((x0, y0), (x1, y1)) r1@((a0, b0), (a1, b1))
  | isNothing (intersect2d r0 r1) = []
  | otherwise =
    let xs = [min x0 a0, max x0 a0, min x1 a1, max x1 a1]
        ys = [min y0 b0, max y0 b0, min y1 b1, max y1 b1]
        possibleBoxes = do
          (y, y') <- zip ys (tail ys)
          (x, x') <- zip xs (tail xs)
          return ((x, y), (x', y'))
        filterFn box = isJust (intersect2d r0 box) || isJust (intersect2d r1 box)
     in filter filterFn possibleBoxes

calcDisjointCubes cube0 cube1
  | isNothing (intersect3d cube0 cube1) = [cube0, cube1]
  | otherwise =
    let possibleCubes = subCubes cube0 cube1
        filterFn cube = isJust (intersect3d cube0 cube) || isJust (intersect3d cube1 cube)
     in filter filterFn possibleCubes

subCubes ((x0, y0, z0), (x1, y1, z1)) ((a0, b0, c0), (a1, b1, c1)) =
  let xs = [min x0 a0, max x0 a0, min x1 a1, max x1 a1]
      ys = [min y0 b0, max y0 b0, min y1 b1, max y1 b1]
      zs = [min z0 c0, max z0 c0, min z1 c1, max z1 c1]
   in do
        (z, z') <- zip zs (tail zs)
        (y, y') <- zip ys (tail ys)
        (x, x') <- zip xs (tail xs)
        return ((x, y, z), (x', y', z'))

isContainedIn ((x0, y0, z0), (x1, y1, z1)) ((a0, b0, c0), (a1, b1, c1)) =
  (x0 <= a0) && (a1 <= x1) && (y0 <= b0) && (b1 <= y1) && (z0 <= c0) && (c1 <= z1)

isDisjoint cube0 cube1 = isNothing (intersect3d cube0 cube1)

partitionRegions cubes cube =
  let go [] cont disj partial = (cont, disj, partial)
      go (c : rest) cont disj partial
        | isContainedIn c cube = go rest (c : cont) disj partial
        | isDisjoint c cube = go rest cont (c : disj) partial
        | otherwise = go rest cont disj (c : partial)
   in go cubes [] [] []

addToDisjoint cubes cube =
  let (contained, disj, partial) = partitionRegions cubes cube
      subs =
        partial >>= \c ->
          filter
            (\s -> isJust (intersect3d c s) && isNothing (intersect3d cube s))
            (subCubes cube c)
      cubes'' = foldr (:) (cube : disj) subs
   in cubes''

removeFromDisjoint cubes cube =
  let (contained, disj, partial) = partitionRegions cubes cube
      subs =
        partial >>= \c ->
          filter
            (\s -> isJust (intersect3d c s) && isNothing (intersect3d cube s))
            (subCubes cube c)
      cubes' = foldr (:) disj subs
   in cubes'

volume ((x0, y0, z0), (x1, y1, z1)) =
  (x1 - x0) * (y1 - y0) * (z1 - z0)

rect1 :: Cube
rect1 = ((0, 0, 0), (2, 2, 2))

rect2 :: Cube
rect2 = ((1, 1, 1), (3, 3, 3))

isInitialInstruction (Instruction _ ((x0, y0, z0), (x1, y1, z1))) =
  all (\x -> -50 <= x && x <= 51) [x0, y0, z0, x1, y1, z1]

countInitialOnCubes :: [Instruction] -> Int
countInitialOnCubes instructions =
  let foldFn onCubes (Instruction isOn cube) =
        if isOn
          then addToDisjoint onCubes cube
          else removeFromDisjoint onCubes cube
   in sum . fmap volume . foldl' foldFn [] . filter isInitialInstruction $ instructions

simpleCountOnCubes :: [Instruction] -> Int
simpleCountOnCubes instructions =
  let rmOvercount (Instruction isOn ((x0, y0, z0), (x1, y1, z1))) =
        Instruction isOn ((x0, y0, z0), (x1 - 1, y1 - 1, z1 - 1))
      foldFn acc (Instruction isOn ((x0, y0, z0), (x1, y1, z1))) =
        let pts = do
              x <- [x0 .. x1]
              y <- [y0 .. y1]
              z <- [z0 .. z1]
              return (x, y, z)
         in if isOn
              then foldr Set.insert acc pts
              else foldr Set.delete acc pts
   in Set.size . foldl' foldFn Set.empty . fmap rmOvercount . filter isInitialInstruction $ instructions

--  in sum . fmap volume . foldl' foldFn [] . filter filterFn $ instructions

-- let foldFn onCubes (Instruction isOn ((xmin, xmax), (ymin, ymax), (zmin, zmax))) =
--       undefined
--  in Set.size $ foldl' foldFn Set.empty instructions

day22a =
  runDay
    "input/day22-test.txt"
    (parserManyLines parserInstruction)
    (Right . countInitialOnCubes)

day22a' =
  runDay
    "input/day22-test.txt"
    (parserManyLines parserInstruction)
    (Right . simpleCountOnCubes)

countOnCubes :: [Instruction] -> Int
countOnCubes instructions =
  let foldFn onCubes (Instruction isOn cube) =
        if isOn
          then addToDisjoint onCubes cube
          else removeFromDisjoint onCubes cube
   in sum . fmap volume . foldl' foldFn [] $ instructions

day22b =
  runDay
    "input/day22.txt"
    (parserManyLines parserInstruction)
    (Right . countOnCubes)
