module Main where

import Data.List
import Data.List.Split
import Data.Map (Map, (!), fromList)

type Image = [[Char]]

type Rules = Map Image Image

rotate = reverse . transpose

getVariations :: Image -> [Image]
getVariations i = concatMap (take 4 . iterate rotate) [i, reverse i]

stepImage :: Rules -> Image -> Image
stepImage r img =
  map concat
    .   transpose
    .   map (r!)
    .   transpose
    .   map (chunksOf n)
    =<< chunksOf n img
 where
  n | even (length img) = 2
    | otherwise         = 3

buildRules :: [(Image, Image)] -> Rules
buildRules s = fromList [ (k, v) | (k', v) <- s, k <- getVariations k' ]

startImg = [".#.", "..#", "###"]

solve ip n =
  length
    $  filter ('#'==)
    $  concat
    $  iterate (stepImage $ buildRules ip) startImg
    !! n

solve1 i = solve i 5
solve2 i = solve i 18

prepareInput = fmap (tup . splitOn " => ") . lines
  where tup a = (splitOn "/" $ head a, splitOn "/" $ last a)

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
