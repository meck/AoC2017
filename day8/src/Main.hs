module Main where

import Data.List
import Data.StringMap (StringMap)
import qualified Data.StringMap as M

type RegName = String

type State = StringMap Int

data Inst = Inst
  { regN :: RegName
  , ins :: Int -> Int
  , cRegN :: RegName
  , cond :: Int -> Bool
  }

assertExist :: RegName -> State -> State
assertExist n = M.insertWith (flip const) n 0

step :: State -> Inst -> State
step s0 i = M.adjust change (regN i) s
 where
  s      = assertExist (cRegN i) $ assertExist (regN i) s0
  change = if cond i (s M.! cRegN i) then ins i else id

readOp :: String -> String -> (Int -> Int)
readOp strI strV = flip i v
 where
  i | strI == "dec" = (-)
    | strI == "inc" = (+)
  v = read strV

readCond :: String -> String -> (Int -> Bool)
readCond strC strV = c v
 where
  c | strC == "==" = (==)
    | strC == "!=" = (/=)
    | strC == "<"  = flip (<)
    | strC == ">"  = flip (>)
    | strC == "<=" = flip (<=)
    | strC == ">=" = flip (>=)
  v = read strV

readInstruction :: String -> Inst
readInstruction s = Inst
  { regN  = r
  , ins   = readOp o v
  , cRegN = cr
  , cond  = readCond co cv
  }
  where (r:o:v:_:cr:co:cv:_) = words s

prepareInput = map readInstruction . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (M.foldr max 0 (foldl' step M.empty input))
  putStrLn $ "Part 2: " ++ show (snd (foldl' go (M.empty, 0) input))
 where
  go (s, acc) i = (next, max acc $ maximum $ M.elems next)
    where next = step s i
