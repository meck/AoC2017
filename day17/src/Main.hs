module Main where

import Data.List

step = 370

type Pos = Int
type Val = Int

positions :: Val -> [(Pos, Val)]
positions n = scanl go (0, 0) [1 .. n]
  where go (pPos, pVal) n = ((pPos + step) `mod` n + 1, n)

buildBuff :: Val -> [Val]
buildBuff n = foldl' ins [] $ positions n
  where ins res (p, v) = let (xs, ys) = splitAt p res in xs ++ v : ys

solve1 :: Val -> Val
solve1 n = head $ tail $ dropWhile (/=n) $ buildBuff (n + 1)

solve2 :: Val -> Val
solve2 = foldl' go 0 . positions
  where go res (pos, val) = if pos == 1 then val else res

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (solve1 2017)
  putStrLn $ "Part 2: " ++ show (solve2 50000000)
