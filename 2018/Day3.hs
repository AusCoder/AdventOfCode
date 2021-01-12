{-# LANGUAGE OverloadedStrings #-}

module Day3 (day3) where

import Common
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

data Box = Box
  { id :: Int,
    x :: Int,
    y :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show)

boxParser :: Parser Box
boxParser = do
  id <- PC.char '#' >> numberParser
  x <- PC.spaces >> PC.char '@' >> PC.spaces >> numberParser
  y <- PC.char ',' >> numberParser
  w <- PC.char ':' >> PC.spaces >> numberParser
  h <- PC.char 'x' >> numberParser
  return $ Box id x y w h

day3 :: IO ()
day3 = do
  content <- TIO.readFile "input/day3.txt"
  print . mapM (simpleRunParser boxParser) . T.lines $ content
  putStrLn "abc"
