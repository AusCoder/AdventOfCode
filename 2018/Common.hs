{-# LANGUAGE TupleSections #-}

module Common
  ( SimpleParser,
    AOCError (..),
    numberParser,
    dayParser,
    hourMinuteParser,
    runSimpleParser,
    pairwiseCombs,
  )
where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.Calendar
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

type SimpleParser a = P.ParsecT T.Text () Identity a

data AOCError = AOCParseError P.ParseError | AOCGenericError String deriving (Show)

numberParser :: SimpleParser Int
numberParser =
  let numberFromDigits = foldl' (\acc n -> acc * 10 + n) 0
   in numberFromDigits . fmap digitToInt <$> P.many1 PC.digit

dayParser :: SimpleParser Day
dayParser = do
  y <- fromIntegral <$> numberParser
  m <- PC.char '-' *> numberParser
  d <- PC.char '-' *> numberParser
  maybe (P.parserFail "invalid day") return $ fromGregorianValid y m d

hourMinuteParser :: SimpleParser TimeOfDay
hourMinuteParser = do
  h <- numberParser
  m <- PC.char ':' *> numberParser
  maybe (P.parserFail "invalid hour min") return $ makeTimeOfDayValid h m 0

runSimpleParser :: SimpleParser a -> T.Text -> Either AOCError a
runSimpleParser p = either (Left . AOCParseError) Right . P.parse p ""

pairwiseCombs :: [a] -> [(a, a)]
pairwiseCombs [] = []
pairwiseCombs [x] = []
pairwiseCombs [x, y] = [(x, y)]
pairwiseCombs (x : xs) = fmap (x,) xs ++ pairwiseCombs xs
