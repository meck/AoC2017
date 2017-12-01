module Main where

import Data.Char

sumOfMatching :: (Eq a, Num a) => [a] -> Int -> a
sumOfMatching xs n =
  sum $ zipWith (\x y -> if x == y then x else 0) xs (drop n $ cycle xs)

prepareInput = map digitToInt . reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  input <- prepareInput <$> readFile "input.txt"
  putStrLn $ "Part1: " ++ show (sumOfMatching input 1)
  putStrLn $ "Part2: " ++ show (sumOfMatching input (length input `div` 2))
