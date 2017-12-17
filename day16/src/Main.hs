module Main where

import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Vector as V

type State = V.Vector Char

data Dance = Spin Int | Exchange Int Int | Partner Char Char deriving Show

doDance :: State -> Dance -> State
doDance s d = case d of
  (Spin n) -> V.drop i s V.++ V.take i s where i = V.length s - n
  (Exchange a b) ->
    V.imap (\i x -> if i == a then s V.! b else if i == b then s V.! a else x) s
  (Partner a b) ->
    V.map (\x -> if x == a then b else if x == b then a else x) s


findCycle :: State -> [Dance] -> Int
findCycle s d = fromMaybe 0 $ elemIndex s $ tail steps
  where steps = iterate (flip (foldl doDance) d) s


readDance :: String -> Dance
readDance (c:xs) =
  let (a1:a2:_) = splitOn "/" xs
  in  case c of
        's' -> Spin (read xs)
        'x' -> Exchange (read a1) (read a2)
        'p' -> Partner (head a1) (head a2)


solve1 = foldl' doDance (V.fromList ['a' .. 'p'])

solve2 cmd = iterate (flip (foldl' doDance) cmd) state !! cycleSize
 where
  state     = V.fromList ['a' .. 'p']
  cycleSize = mod 1000000000 $ findCycle state cmd + 1

prepareInput = map readDance . splitOn ","

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
