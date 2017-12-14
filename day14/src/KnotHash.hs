module KnotHash (
  hashKnot,
  hashKnotInt
) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Bits
import Numeric

type Index = Int
type Length = Int

rev :: Index -> Length -> [Int] -> [Int]
rev i l xs = rotate (negate i) $ flipTo l $ rotate i xs
 where
  rotate i xs = take (length xs) $ drop (length xs + i) $ cycle xs
  flipTo l xs = reverse (take l xs) ++ drop l xs

hashOnce :: (Index, Int, [Int]) -> [Int] -> (Index, Int, [Int])
hashOnce = foldl'
  ( \(pos, skip, lst) l ->
    ((pos + l + skip) `mod` length lst, skip + 1, rev pos l lst)
  )

addMagic = (++[17, 31, 73, 47, 23])

runHash xs =
  (\(_, _, x) -> x) $ iterate (`hashOnce`xs) (0, 0, [0 .. 255]) !! 64

xorList = map (foldl' xor 0) . chunksOf 16

hexStr = pad . (`showHex`"")
  where pad s = if length s < 2 then pad ("0" ++ s) else s

hashKnot :: String -> String
hashKnot = concatMap hexStr . xorList . runHash . addMagic . map ord

hashKnotInt :: String -> [Int]
hashKnotInt = xorList . runHash . addMagic . map ord

