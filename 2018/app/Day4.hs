module Day4 where

import Common
import Data.Time
import Debug.Trace
import qualified Data.Map.Strict as Map
import Data.List (sortBy, maximumBy)
import Data.Ord(comparing)
import Text.Parsec

data GuardEventType =
  GuardStarts Int
  | GuardFallsAsleep
  | GuardWakesUp
  deriving (Show)
data GuardEvent = GuardEvent
  { eventTime :: UTCTime
  , eventMinute :: Int
  , eventType :: GuardEventType
  } deriving (Show)

parserGuardEvent :: StringParser GuardEvent
parserGuardEvent = do
  char '['
  et <- read <$> many (digit <|> char '-')
  h <- space >> parserInt
  m <- char ':' >> parserInt
  char ']' >> space
  eType <- choice [
                  string "Guard #" >> parserInt >>= \i -> const (GuardStarts i) <$> string " begins shift",
                  const GuardFallsAsleep <$> string "falls asleep",
                  const GuardWakesUp <$> string "wakes up"
                ]
  return $ GuardEvent (UTCTime et (fromInteger . toInteger $ 60 * h + m)) m eType

parserGuardEvents = many (parserGuardEvent >>= \g -> g <$ endOfLine)

getEventsByGuard :: [GuardEvent] -> Map.Map Int [GuardEvent]
getEventsByGuard events =
  let go curGuard acc [] = acc
      go curGuard acc (x:xs) =
        let goInsert guardId ev = Map.insert guardId (ev : (fromMaybe [] $ Map.lookup guardId acc)) acc
         in case eventType x of
              GuardStarts guardId -> go (Just guardId) (goInsert guardId x) xs
              _ -> go curGuard (goInsert (fromMaybe undefined curGuard) x) xs
  in fmap reverse $ go Nothing Map.empty $ sortBy (comparing eventTime) events

calculateMinuteCounts :: [GuardEvent] -> Map.Map Int Int
calculateMinuteCounts events =
  let go acc _ [] = acc
      go acc start (x:xs) =
        let evMin = eventMinute x
         in case eventType x of
              GuardFallsAsleep -> go acc (Just evMin) xs
              GuardWakesUp ->
                let
                  foldFn x m = Map.insert x ((+) 1 . fromMaybe 0 . Map.lookup x $ m) m
                  acc' = foldr foldFn acc [fromMaybe undefined start .. evMin - 1]
                 in go acc' Nothing xs
              _ -> go acc start xs
  in go Map.empty Nothing events

doPart cmpFn events =
  let eventsByGuard = getEventsByGuard events
      maxMinute cs = fst . maximumBy (comparing $ \(m, c)-> c) . Map.toList $ cs
      (i, counts) = maximumBy cmpFn
                      . fmap (\(i, evs) -> (i, calculateMinuteCounts evs))
                      . Map.toList
                      $ eventsByGuard
  in i * maxMinute counts

part1 = doPart (comparing $ \(_, cs) -> sum cs)

day4a = runDayWithParser "input/day4.txt" parserGuardEvents (Right . part1)

part2 = doPart (comparing $ \(_, cs) -> if Map.size cs == 0 then -1 else maximum cs)

day4b = runDayWithParser "input/day4.txt" parserGuardEvents (Right . part2)
