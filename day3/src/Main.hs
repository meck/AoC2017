module Main where

import Data.Char
import Data.List
import Data.Maybe


pointInUlam :: (Integral a) => a -> (a, a)
pointInUlam n | n >= m0 - t = (k - (m0 - n), -k)
              | n >= m1 - t = (-k, (-k) + (m1 - n))
              | n >= m2 - t = ((-k) + (m2 - n), k)
              | otherwise   = (k, k - (m2 - n - t))
 where
  k  = ceiling $ (sqrt (fromIntegral n) - 1) / 2
  t0 = 2 * k + 1
  m0 = t0 ^ 2
  m1 = m0 - t
  m2 = m1 - t
  t  = t0 - 1


idxOfCord :: (Num a, Ord a) => (a, a) -> a
idxOfCord (x, y) | y >= 0 && (x >= 1 - y && x < y) = (2 * y) ^ 2 - x - y + 1
                 | y < 0 && (x >= y && x <= (-y))  = (2 * y - 1) ^ 2 + x + y
                 | x > 0                           = (2 * x - 1) ^ 2 + x + y
                 | otherwise                       = ((-2) * x) ^ 2 - x - y + 1

-- TODO: Slow af
valueOfCord (  0, 0) = 1
valueOfCord p@(x, y) = sum $ map
  (\pn -> if idxOfCord pn < idxOfCord p then valueOfCord pn else 0)
  neighbours
 where
  neighbours =
    [ (x    , y + 1)
    , (x    , y - 1)
    , (x + 1, y)
    , (x - 1, y)
    , (x + 1, y + 1)
    , (x + 1, y - 1)
    , (x - 1, y + 1)
    , (x - 1, y - 1)
    ]


manhattanDist (x, y) = abs x + abs y

prepareInput :: String -> Int
prepareInput = read

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (manhattanDist $ pointInUlam input)
  putStrLn $ "Part 2: " ++ show
    (head $ dropWhile (<input) $ map (valueOfCord . pointInUlam) [1 ..])

