module Common
  ( Parser,
    AOCError (..),
    numberParser,
    simpleRunParser,
  )
where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

type Parser a = P.ParsecT T.Text () Identity a

data AOCError = AOCParseError P.ParseError | AOCGenericError String deriving (Show)

numberParser :: Parser Int
numberParser =
  let numberFromDigits = foldl' (\acc n -> acc * 10 + n) 0
   in numberFromDigits . fmap digitToInt <$> P.many1 PC.digit

simpleRunParser :: Parser a -> T.Text -> Either AOCError a
simpleRunParser p = either (Left . AOCParseError) Right . P.parse p ""
