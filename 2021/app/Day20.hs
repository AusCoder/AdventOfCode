module Day20 where

import Common
import Control.Monad (join)
import Data.Array
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec (char, endOfLine, many1, unexpected, (<|>))

data ImageAndAlgo = ImageAndAlgo
  { getAlgo :: Array Int Char,
    getLitPixels :: Set.Set (Int, Int),
    getBackground :: Char
  }
  deriving (Show)

parserPuzzle = do
  algo <- many1 pixelP
  let algoArr = listArray (0, length algo - 1) algo
  endOfLine
  endOfLine
  image <- many1 (many1 pixelP <* endOfLine)
  let w = length (head image)
  if all ((== w) . length) image then return () else unexpected "different lengths"
  let litPixels =
        foldr Set.insert Set.empty $
          zip [0 ..] image >>= \(y, row) ->
            zip [0 ..] row >>= \(x, p) ->
              [(y, x) | p == '#']
  return $ ImageAndAlgo algoArr litPixels '.'
  where
    pixelP = char '.' <|> char '#'

adjacentPoints (x, y) =
  [-1 .. 1] >>= \dx -> [-1 .. 1] >>= \dy -> [(x + dx, y + dy)]

getPixel (ImageAndAlgo _ litPixels background) pt =
  if Set.member pt litPixels then flipPixel background else background

showImage imageAndAlgo@(ImageAndAlgo _ litPixels _) =
  let ((xmin, xmax), (ymin, ymax)) = litPixelBounds litPixels
   in [ymin - 2 .. ymax + 2] >>= \y ->
        [getPixel imageAndAlgo (y, x) | x <- [xmin - 2 .. xmax + 2]] ++ "\n"

flipPixel '.' = '#'
flipPixel '#' = '.'
flipPixel _ = error "flipPixel: unknown char"

toBin '.' = '0'
toBin '#' = '1'
toBin _ = error "toBin: unknown char"

algoValue :: ImageAndAlgo -> (Int, Int) -> Char
algoValue imageAndAlgo@(ImageAndAlgo algo _ _) point =
  let cs = toBin . getPixel imageAndAlgo <$> adjacentPoints point
   in algo ! binToInt cs

litPixelBounds litPixels =
  let pts = Set.toList litPixels
      xs = fmap snd pts
      ys = fmap fst pts
   in ((minimum xs, maximum xs), (minimum ys, maximum ys))

applyAlgo imageAndAlgo@(ImageAndAlgo algo litPixels background) =
  let ((xmin, xmax), (ymin, ymax)) = litPixelBounds litPixels
      background'
        | (background == '.') && (algo ! 0 == '#') = '#'
        | (background == '#') && (algo ! 511 == '.') = '.'
        | otherwise = background

      litPixels' =
        foldr Set.insert Set.empty $
          [ymin - 1 .. ymax + 1] >>= \y ->
            [xmin - 1 .. xmax + 1] >>= \x ->
              [(y, x) | algoValue imageAndAlgo (y, x) /= background']
   in imageAndAlgo {getLitPixels = litPixels', getBackground = background'}

enhanceAndCount :: Int -> ImageAndAlgo -> Int
enhanceAndCount 0 imageAndAlgo = Set.size . getLitPixels $ imageAndAlgo
enhanceAndCount n imageAndAlgo = enhanceAndCount (n - 1) (applyAlgo imageAndAlgo)

day20a =
  runDay "input/day20.txt" parserPuzzle (Right . enhanceAndCount 2)

day20b =
  runDay "input/day20.txt" parserPuzzle (Right . enhanceAndCount 50)
