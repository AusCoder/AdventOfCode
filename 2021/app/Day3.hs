module Day3 where

import Common
import Data.Foldable (maximumBy, minimumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Debug.Trace
import Text.Parsec

parserDiagnostic :: StringParser String
parserDiagnostic = many (char '0' <|> char '1')

countChars :: String -> Map.Map Char Int
countChars = foldr (\c -> Map.insertWith (+) c 1) Map.empty

maxChar :: Map.Map c Int -> c
maxChar = fst . maximumBy (comparing snd) . Map.toList

minChar :: Map.Map c Int -> c
minChar = fst . minimumBy (comparing snd) . Map.toList

calculatePowerConsumption :: [String] -> Integer
calculatePowerConsumption diagnostics =
  let calc :: (Map.Map Char Int -> Char) -> [String] -> String
      calc chooseCharFn lns =
        let m = fmap (chooseCharFn . countChars) . mapM headMay $ lns
            tails = fmap tail lns
         in maybe [] (\c -> c : calc chooseCharFn tails) m
      gamma = calc maxChar diagnostics
      epsilon = calc minChar diagnostics
   in toInteger (binToInt gamma) * toInteger (binToInt epsilon)

day3a =
  runDay
    "input/day3.txt"
    (parserManyLines parserDiagnostic)
    (Right . calculatePowerConsumption)

calculateLifeSupport :: [String] -> Maybe Integer
calculateLifeSupport diagnostics =
  let go chooseCharFn lns ts =
        mapM headMay ts >>= \heads ->
          let counts = countChars heads
              count0 = fromMaybe 0 $ Map.lookup '0' counts
              count1 = fromMaybe 0 $ Map.lookup '1' counts
              c = chooseCharFn count0 count1
              fltd = filter (\(_, _, h) -> h == c) $ zip3 lns ts heads
              ts' = tail <$> fmap (\(_, x, _) -> x) fltd
              lns' = fmap (\(x, _, _) -> x) fltd
           in if length lns' == 1
                then headMay lns'
                else go chooseCharFn lns' ts'

      oxChooseCharFn count0 count1 = if count1 >= count0 then '1' else '0'
      co2ChooseCharFn count0 count1 = if count0 <= count1 then '0' else '1'
   in do
        ox <- go oxChooseCharFn diagnostics diagnostics
        co2 <- go co2ChooseCharFn diagnostics diagnostics
        return $ toInteger (binToInt ox) * toInteger (binToInt co2)

day3b =
  runDay
    "input/day3.txt"
    (parserManyLines parserDiagnostic)
    (Right . calculateLifeSupport)
