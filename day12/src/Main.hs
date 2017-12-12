module Main where

import Data.Char
import Data.List
import Data.List.Split
import Control.Monad
import qualified Data.Map.Strict as M

data Program = Program
  { num :: Int
  , pipes :: [Program]
  }

instance Eq Program where
  a == b = num a == num b

instance Ord Program where
  compare a b = compare (num a) (num b)

buildTree :: Int -> M.Map Int [Int] -> Program
buildTree r i = Program r $ map (`buildTree`i) (i M.! r)

findGroup :: Program -> [Program]
findGroup p = go p []
 where
  go p found = sort $ foldr
    (\x acc -> if x `elem` acc then acc else go x acc)
    (p : found)
    (pipes p)

solve1 = length . findGroup . buildTree 0
solve2 m = length $ M.mapKeys (head . findGroup . flip buildTree m) m

prepareInput :: String -> M.Map Int [Int]
prepareInput =
  M.fromList . map (liftM2 (,) head tail . map read . words) . lines . filter
    (liftM2 (||) isNumber isSpace)

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
