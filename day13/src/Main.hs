module Main where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as Map
import Control.Monad
import Data.Char
import Data.Maybe

type Pos = Int
type Range = Int
type Firewall = IntMap Range
type Ticks = Int
type Delay = Ticks

data State = State Firewall Ticks

costOfState :: Delay -> State -> Maybe Int
costOfState d (State fw t) =
  let pos = Map.lookup t fw
  in  case posOfScan (t + d) <$> pos of
        (Just 0) -> (*t) <$> pos
        _        -> Nothing

posOfScan :: Ticks -> Range -> Pos
posOfScan t r = if fst p `mod` 2 == 0 then snd p else abs (snd p - r + 1)
  where p = divMod t (r - 1)

allStates :: Firewall -> [State]
allStates fw = [ State fw t | t <- [0 .. (fst $ Map.findMax fw)] ]

solve1 = sum . mapMaybe (costOfState 0) . allStates
solve2 fw = length $ takeWhile
  (not . all isNothing)
  [ map (costOfState d) $ allStates fw | d <- [0 ..] ]

prepareInput :: String -> Firewall
prepareInput =
  Map.fromList . map ((\(x:y:_) -> (x, y)) . map read . words) . lines . filter
    (liftM2 (||) isNumber isSpace)

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
