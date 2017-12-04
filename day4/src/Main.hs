module Main where

import Data.List
import qualified Data.Set as Set

-- Quicker then `elem`
hasDuplicate :: Ord a => [a] -> Bool
hasDuplicate xs = length xs == length (Set.fromList xs)

isAnagram :: Ord a => [a] -> [a] -> Bool
isAnagram xs ys = sort xs == sort ys

hasAnagrams :: Ord a => [[a]] -> Bool
hasAnagrams (x:xs) =
  foldr (\x0 acc -> isAnagram x x0 || acc) False xs || hasAnagrams xs
hasAnagrams [] = False

prepareInput = map words . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "part1: " ++ show (length $ filter (not . hasDuplicate) input)
  putStrLn $ "part2: " ++ show (length $ filter (not . hasAnagrams) input)

