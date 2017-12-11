
module Main where

import Data.Char
import Data.List.Split
import Data.List

data Pos = Pos { q :: Int
                , r :: Int }

orig = Pos 0 0

s :: Pos -> Int
s p = negate (q p) - r p

dist :: Pos -> Pos -> Int
dist a b = maximum [abs (q a - q b), abs (r a - r b), abs (s a - s b)]

instance Ord Pos where
  compare a b = dist a orig `compare` dist b orig

instance Eq Pos where
  a == b =  dist a orig == dist b orig

data Direction = N | NE | SE | S | SW | NW

getDir :: String -> Direction
getDir str = case str of
  "n"  -> N
  "ne" -> NE
  "se" -> SE
  "s"  -> S
  "sw" -> SW
  "nw" -> NW

step :: Pos -> Direction -> Pos
step p d = case d of
  N  -> p { q = succ (q p) }
  NE -> p { r = succ (r p) }
  SE -> p { q = pred (q p), r = succ (r p) }
  S  -> p { q = pred (q p) }
  SW -> p { r = pred (r p) }
  NW -> p { q = succ (q p), r = pred (r p) }

solve1 = dist orig . foldl' step orig . map getDir
solve2 = dist orig . maximum . scanl' step orig . map getDir

prepareInput = splitOn "," . reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
