module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Bits
import Numeric

type Idx = Int
type OpL = Int


-- Part 1
step :: Idx -> OpL -> [Int] -> [Int]
step i l xs = rotate (negate i) $ flipTo l $ rotate i xs
 where
  rotate i xs = take (length xs) $ drop (length xs + i) $ cycle xs
  flipTo l xs = reverse (take l xs) ++ drop l xs

hashOnce :: (Idx, Int, [Int]) -> [Int] -> (Idx, Int, [Int])
hashOnce = foldl'
  ( \(pos, skip, lst) l ->
    ((pos + l + skip) `mod` length lst, skip + 1, step pos l lst)
  )

hash :: Int -> [Int] -> [Int]
hash len = (\(_, _, xs) -> xs) . hashOnce (0, 0, [0 .. (len - 1)])

prepareInput :: String -> [Int]
prepareInput = map read . splitOn ","

-- Part 2
addMagic = (++[17, 31, 73, 47, 23])

runHash xs =
  (\(_, _, x) -> x) $ iterate (`hashOnce`xs) (0, 0, [0 .. 255]) !! 64

xorList = map (foldl' xor 0) . chunksOf 16

hexStr = pad . (`showHex`"")
  where pad s = if length s < 2 then pad ("0" ++ s) else s

hashKnot :: String -> String
hashKnot = concatMap hexStr . xorList . runHash . addMagic . map ord

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (product $ take 2 $ hash 256 input)
  input2 <- init <$> readFile "Input.txt"
  putStrLn $ "Part 2: " ++ show (hashKnot input2)
