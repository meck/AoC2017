module Main where

import Data.List
import Data.Maybe

intQuote :: Integral t => t -> t -> Maybe t
intQuote x y | snd res == 0 = Just (fst res)
             | otherwise    = Nothing
  where res = max x y `divMod` min x y

intQuotes :: Integral t => [t] -> [Maybe t]
intQuotes []     = []
intQuotes (x:xs) = map (intQuote x) xs ++ intQuotes xs

part1 = sum . map (\x -> maximum x - minimum x)
part2 = sum . map (head . catMaybes . intQuotes)

prepareInput = map (map read . words) . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part1: " ++ show (part1 input)
  putStrLn $ "Part2: " ++ show (part2 input)
