module Main where

import Data.Char
import Data.List
import Data.List.Split
import Control.Monad

data Program = Program
  { num :: Int
  , pipes :: [Program]
  }

instance Eq Program where
  a == b = num a == num b

instance Ord Program where
  compare a b = compare (num a) (num b)

buildTree :: Int -> [(Int, [Int])] -> Program
buildTree r i = Program r $ map (`buildTree`i) (children r)
  where children n = snd $ head $ filter ((==) n . fst) i

findInGroup :: Program -> [Program]
findInGroup prg = go prg []
 where
  go p found = sort $ foldr
    (\x acc -> if x `elem` acc then acc else go x acc)
    (p : found)
    (pipes p)

solve1 = length . findInGroup . buildTree 0

solve2 a =
  length $ group $ sort $ map (head . findInGroup . flip buildTree a . fst) a

prepareInput :: String -> [(Int, [Int])]
prepareInput =
  map (liftM2 (,) head tail) . map (map read . words) . lines . filter
    (liftM2 (||) isNumber isSpace)

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
