module Main where

import Data.Char
import Data.List

filterGarbage :: String -> (Int, String)
filterGarbage = (\(_, _, x, y) -> (x, y)) . foldl' go (False, False, 0, [])
 where
  go (isGarbage, ignore, count, res) c
    | not isGarbage && c == '<' = (True, ignore, count, res)
    | not isGarbage             = (isGarbage, ignore, count, res ++ [c])
    | ignore                    = (isGarbage, False, count, res)
    | c == '!'                  = (isGarbage, True, count, res)
    | c == '>'                  = (False, ignore, count, res)
    | otherwise                 = (isGarbage, ignore, count + 1, res)

totScore :: String -> Int
totScore = sum . snd . foldl' go (0, [])
 where
  go (s, res) c | c == '{'  = (s + 1, res)
                | c == '}'  = (s - 1, s : res)
                | otherwise = (s, res)

solve1 = totScore . snd . filterGarbage
solve2 = fst . filterGarbage

prepareInput = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)

