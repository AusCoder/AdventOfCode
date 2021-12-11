module Day10 where

import Common
import Data.Maybe (isNothing, maybeToList)
import Data.Sort (sort)
import Debug.Trace
import Text.Parsec (char, many, oneOf)

parserChunks = parserManyLines $ many (oneOf "[](){}<>")

buildValidStack :: String -> (String, String)
buildValidStack origLn =
  let go stack [] = (stack, [])
      go stack (h : rest) =
        if h `elem` "[({<"
          then go (h : stack) rest
          else case stack of
            [] -> (stack, h : rest)
            (x : xs) -> case (x, h) of
              ('[', ']') -> go xs rest
              ('(', ')') -> go xs rest
              ('{', '}') -> go xs rest
              ('<', '>') -> go xs rest
              (_, _) -> (stack, h : rest)
   in go [] origLn

findIllegalChar :: String -> Maybe Char
findIllegalChar origLn =
  let (_, invalidStr) = buildValidStack origLn
   in headMay invalidStr

errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137
errorScore _ = error "unknown char"

calculateErrorScore :: [String] -> Int
calculateErrorScore chunks =
  let errorChars = chunks >>= maybeToList . findIllegalChar
   in sum . fmap errorScore $ errorChars

day10a =
  runDay
    "input/day10.txt"
    parserChunks
    (Right . calculateErrorScore)

closingChar '[' = ']'
closingChar '(' = ')'
closingChar '{' = '}'
closingChar '<' = '>'
closingChar _ = error "unknown char"

findCompletion :: String -> String
findCompletion origLn =
  let (stack, _) = buildValidStack origLn
   in foldr ((:) . closingChar) [] stack

calculateCompletionScore :: String -> Int
calculateCompletionScore =
  let charScore ')' = 1
      charScore ']' = 2
      charScore '}' = 3
      charScore '>' = 4
      charScore _ = error "unknown char"
   in foldl (\acc c -> acc * 5 + charScore c) 0

calculateIncompleteScore :: [String] -> Int
calculateIncompleteScore chunks =
  let completeOrIncompleteChunks = filter (isNothing . findIllegalChar) chunks
      completions = filter (not . null) . fmap findCompletion $ completeOrIncompleteChunks
      sortedScores = sort . fmap calculateCompletionScore $ completions
   in sortedScores !! (length sortedScores `div` 2)

day10b =
  runDay
    "input/day10.txt"
    parserChunks
    (Right . calculateIncompleteScore)
