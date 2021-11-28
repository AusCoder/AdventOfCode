{-# LANGUAGE TupleSections #-}

module Common where

import Control.Monad ((>=>))
import Text.Parsec

data AOCError
  = AOCParseError ParseError
  | AOCCustomError String
  deriving (Show)

maybeToErr :: String -> Maybe a -> Either AOCError a
maybeToErr msg = maybe (Left $ AOCCustomError msg) Right

type StringParser a = Parsec String () a

runStringParser :: StringParser a -> String -> Either AOCError a
runStringParser p = either (Left . AOCParseError) Right . runParser p () "string source"

parserInt :: StringParser Int
parserInt = read <$> many1 digit

parserSignedInt :: StringParser Int
parserSignedInt = do
  s <- oneOf "+-"
  let s' = if s == '+' then 1 else -1
  (* s') <$> parserInt

printResult :: (Show a) => Either AOCError a -> IO ()
printResult (Left err) = putStrLn $ "Got an error: " ++ show err
printResult (Right x) = print x

runDay :: (Show a) => String -> (String -> Either AOCError a) -> IO ()
runDay inputFilename fn = do
  ls <- readFile inputFilename
  printResult $ fn ls

runDayWithParser :: (Show a) => String -> StringParser b -> (b -> Either AOCError a) -> IO ()
runDayWithParser inputFilename parser fn =
  runDay inputFilename (runStringParser parser >=> fn)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) =
  let fn (c : cur) a as = (a, c) : fn cur a as
      fn [] _ (a : as) = fn as a as
      fn [] _ [] = []
   in fn xs x xs

-- here is a list append version:
-- pairs [] = []
-- pairs (x : xs) = foldr ((:) . (x,)) [] xs ++ pairs xs

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x
