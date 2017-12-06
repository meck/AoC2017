module Main where

import Data.Maybe
import Data.List
import qualified Data.Sequence as S

type State = (S.Seq Int)
type Idx = Int

largest :: State -> (Int, Idx)
largest =
  S.foldlWithIndex (\acc@(aV, aI) i v -> if v > aV then (v, i) else acc) (0, 0)

step :: State -> (Int, Idx) -> State
step s (v, i) = go (S.update i 0 s) (v, i + 1)
 where
  len = S.length s
  go s0 (v0, i0) | v0 == 0   = s0
                 | i0 >= len = go s0 (v0, i0 - len)
                 | otherwise = go (S.adjust (+1) i0 s0) (v0 - 1, i0 + 1)

run :: [State] -> State -> (Int, Int)
run ss s | s `elem` ss = (length ss, length ss - fromJust (elemIndex s ss))
         | otherwise   = run (ss ++ [s]) (step s (largest s))

prepareInput :: String -> State
prepareInput = S.fromList . map read . words

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  let res = run [] input
  putStrLn $ "Part1: " ++ show (fst res)
  putStrLn $ "Part2: " ++ show (snd res)

