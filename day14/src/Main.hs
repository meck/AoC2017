module Main where

import KnotHash
import Data.Char
import Data.Bits (testBit, popCount)
import Data.Graph (stronglyConnComp)
import Data.Matrix
import Numeric (readHex)

keyStrings i = map (\x -> i ++ "-" ++ show x) [0 .. 127]

-- part 1
solve1 = sum . map (sum . map popCount . hashKnotInt) . keyStrings

-- part2
hexAsciiToBools :: String -> [Bool]
hexAsciiToBools = concatMap
  ( \c ->
    reverse [ testBit (fst $ head (readHex [c]) :: Int) p | p <- [0 .. 3] ]
  )

makeMtx :: [String] -> Matrix Bool
makeMtx = fromLists . map (hexAsciiToBools . hashKnot)

getSCCs m = stronglyConnComp
  [ (pos, pos, getNeigh m pos) | pos <- allMtxPos, m ! pos ]
 where
  allMtxPos = [ (i, j) | i <- [1 .. (nrows m)], j <- [1 .. (ncols m)] ]
  getNeigh m (i, j) = filter (\(i0, j0) -> safeGet i0 j0 m == Just True)
                             [(i + 1, j), (i, j - 1), (i - 1, j), (i, j + 1)]

solve2 = length . getSCCs . makeMtx . keyStrings


prepareInput = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
