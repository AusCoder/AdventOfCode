{-# LANGUAGE OverloadedStrings #-}

module Day4 (day4) where

import Common
import Data.List (maximumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Sort (sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Time.LocalTime
import Debug.Trace
import Safe
import Text.Parsec as P
import qualified Text.Parsec.Char as PC

data Event = Event
  { datetime :: LocalTime,
    eventType :: EventType
  }
  deriving (Show)

data EventType = BeginShift Int | FallAsleep | WakeUp deriving (Show)

eventMin :: Event -> Int
eventMin = todMin . localTimeOfDay . datetime

localTimeParser :: SimpleParser LocalTime
localTimeParser = do
  d <- dayParser
  t <- PC.char ' ' *> hourMinuteParser
  return $ LocalTime d t

eventTypeParser :: SimpleParser EventType
eventTypeParser =
  let b = BeginShift <$> (PC.string "Guard #" *> numberParser <* PC.string " begins shift")
      w = PC.string "wakes up" *> return WakeUp
      f = PC.string "falls asleep" *> return FallAsleep
   in P.try b <|> P.try w <|> P.try f

eventParser :: SimpleParser Event
eventParser = do
  dt <- PC.char '[' *> localTimeParser
  et <- PC.char ']' *> PC.space *> eventTypeParser
  return $ Event dt et

data SleepTime = SleepTime Int Int deriving (Show)

-- Using unsafe maximum's here and other places
mostSleptCountWithMinute :: [SleepTime] -> (Int, Int)
mostSleptCountWithMinute times =
  let count c [] _ = c
      count c (SleepTime x y : rest) minute =
        count (if minute >= x && minute < y then c + 1 else c) rest minute
   in maximum . fmap (\m -> (count 0 times m, m)) $ [0 .. 59]

mostSleptMinute :: [SleepTime] -> Int
mostSleptMinute = snd . mostSleptCountWithMinute

-- This is a bit nuts
-- Nicer to write 2 smaller list parse fns
sleepTimesById :: [Event] -> Either AOCError (M.Map Int [SleepTime])
sleepTimesById =
  let err m es = Left . AOCGenericError $ "Failed to build sleep times. Map: " ++ show m ++ " Events: " ++ show es
      go (Just i) Nothing m [] = Right m
      go (Just i) Nothing m (e : rest) =
        case eventType e of
          BeginShift newI -> go (Just newI) Nothing m rest
          FallAsleep -> go (Just i) (Just . eventMin $ e) m rest
          _ -> err m (e : rest)
      go (Just i) (Just f) m (e : rest) =
        case eventType e of
          WakeUp ->
            let prev = fromMaybe [] $ M.lookup i m
                newM = M.insert i (SleepTime f (eventMin e) : prev) m
             in go (Just i) Nothing newM rest
          _ -> err m (e : rest)
      go Nothing _ m (e : rest) =
        case eventType e of
          BeginShift newI -> go (Just newI) Nothing m rest
          _ -> err m (e : rest)
      go _ _ m es = err m es
   in go Nothing Nothing M.empty

part1Candidate :: M.Map Int [SleepTime] -> Int
part1Candidate =
  let dur (SleepTime x y) = y - x
      duration = sum . fmap dur
      idTimesMostSleptMin i sts = i * mostSleptMinute sts
   in uncurry idTimesMostSleptMin . maximumBy (comparing $ duration . snd) . M.toList

runPart :: (M.Map Int [SleepTime] -> Int) -> T.Text -> Either AOCError Int
runPart candidateFn t =
  let parseFn = fmap (sortOn datetime) . mapM (runSimpleParser eventParser) . T.lines
   in parseFn t >>= fmap candidateFn . sleepTimesById

part1 :: T.Text -> Either AOCError Int
part1 = runPart part1Candidate

part2Candidate :: M.Map Int [SleepTime] -> Int
part2Candidate =
  let countMinuteId i times = let (c, m) = mostSleptCountWithMinute times in (c, m, i)
      minuteTimesId (_, m, i) = m * i
   in minuteTimesId . maximum . fmap (uncurry countMinuteId) . M.toList

part2 :: T.Text -> Either AOCError Int
part2 = runPart part2Candidate

day4 :: IO ()
day4 = do
  content <- TIO.readFile "input/day4.txt"
  print . part1 $ content
  print . part2 $ content
