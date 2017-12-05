module Main where

import Data.Char
import qualified Data.Sequence as S

type IP = Int
type IC = Int
type JumpLst = S.Seq Int
data State = State {ic::IC, ip::IP, jmps::JumpLst}

step :: (Int -> Int) -> State -> Either IC State
step f (State ic ip jmps)
  | newIp < 1 || newIp > (length jmps - 1) = Left ic
  | otherwise = Right (State (ic + 1) newIp (S.update ip (f inst) jmps))
 where
  inst  = S.index jmps ip
  newIp = inst + ip

run :: (State -> Either IC State) -> State -> Int
run f s = case f s of
  (Left  c ) -> c
  (Right s1) -> run f s1

prepareInput :: String -> S.Seq Int
prepareInput = S.fromList . map read . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part2: " ++ show (run (step (+1)) $ State 0 0 input)
  putStrLn $ "Part2: " ++ show
    (run (step (\x -> if x >= 3 then x - 1 else x + 1)) $ State 0 0 input)
