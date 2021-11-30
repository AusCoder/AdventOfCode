{-# LANGUAGE TupleSections #-}

module Common (module Common, fromMaybe) where

import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)
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

parserManyLines :: StringParser a -> StringParser [a]
parserManyLines p = many (p >>= \x -> x <$ endOfLine)

parserInt :: StringParser Int
parserInt = read <$> many1 digit

parserSignedInt :: StringParser Int
parserSignedInt =
  (try (char '-') <|> return '+') >>= \s ->
    let sign = if s == '-' then -1 else 1
     in (*) sign <$> parserInt

printResult :: (Show a) => Either AOCError a -> IO ()
printResult (Left err) = putStrLn $ "Got an error: " ++ show err
printResult (Right x) = print x

runDay :: (Show a) => String -> StringParser b -> (b -> Either AOCError a) -> IO ()
runDay inputFilename p fn = do
  ls <- readFile inputFilename
  printResult . (runStringParser p >=> fn) $ ls

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

headErr :: [a] -> Either AOCError a
headErr = maybeToErr "head failed" . headMay
