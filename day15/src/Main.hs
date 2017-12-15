module Main where

import Data.Bits
import Data.Word

genDiv = 2147483647
genDivP = 31
genAFact = 16807
genBFact = 48271

-- Part 1
genStepOne :: Int -> Int -> [Int]
-- genDiv is mersenne prime (2 ^ genDivP) - 1
-- optimize https://ariya.io/2007/02/modulus-with-mersenne-prime
-- genStepOne fact = iterate (\x -> (fact * x) `rem` genDiv)
genStepOne fact = iterate (modMers . (*) fact)
 where
  modMers k = if i >= genDiv then i - genDiv else i
    where i = (k .&. genDiv) + (k `shiftR` genDivP)

pairsOne :: (Int, Int) -> [(Int, Int)]
pairsOne (a, b) = drop 1 $ zip (genStepOne genAFact a) (genStepOne genBFact b)

-- Part 2
genStepTwo :: Int -> Int -> Int -> [Int]
genStepTwo fact evenDiv start =
  [ i | i <- genStepOne fact start, i `rem` evenDiv == 0 ]

pairsTwo :: (Int, Int) -> [(Int, Int)]
pairsTwo (a, b) =
  drop 1 $ zip (genStepTwo genAFact 4 a) (genStepTwo genBFact 8 b)

judge :: (Int, Int) -> Int
judge (a, b) = if go a `xor` go b == 0 then 1 else 0
  where go x = fromIntegral x :: Word16

solve1 = sum . take 40000000 . map judge . pairsOne
solve2 = sum . take 5000000 . map judge . pairsTwo

main :: IO ()
main = do
  let input = (699, 124)
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
