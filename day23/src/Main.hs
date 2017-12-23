module Main where

import Data.Char
import qualified Data.Map as M
import Data.Numbers.Primes

type Reg = Char

data Val
  = Lit Int
  | Register Reg

instance Read Val where
  readsPrec _ s@(x:_) =
    if isLetter x
      then [(Register x, [])]
      else [(Lit (read s :: Int), [])]
  readsPrec _ _ = []

data Ins
  = Set Reg
        Val
  | Sub Reg
        Val
  | Mul Reg
        Val
  | Jnz Val
        Val

instance Read Ins where
  readsPrec _ input =
    case words input of
      [cmd, a1, a2] ->
        let r = head a1
            v = read a2 :: Val
        in case cmd of
             "set" -> [(Set r v, [])]
             "sub" -> [(Sub r v, [])]
             "mul" -> [(Mul r v, [])]
             "jnz" -> [(Jnz (read a1 :: Val) v, [])]

type Regs = M.Map Reg Int

type PC = Int

valR :: Regs -> Val -> Int
valR r v = case v of
  (Register x) -> r M.! x
  (Lit      x) -> x

data State =
  S Regs
    PC
    [Ins]

startingState = S (M.fromList [ (r, 0) | r <- ['a' .. 'h'] ]) 0

stepState :: State -> Maybe State
stepState (S regs pc ins) | pc < 0 || pc > (length ins - 1) = Nothing
                          | otherwise = Just (S newRegs newPc ins)
 where
  newRegs = case ins !! pc of
    (Set r v) -> M.insert r (val v) regs
    (Sub r v) -> M.adjust (flip (-) (val v)) r regs
    (Mul r v) -> M.adjust (*val v) r regs
    _         -> regs
  newPc = case ins !! pc of
    (Jnz c v) -> if val c /= 0 then pc + val v else succ pc
    _         -> succ pc
  val = valR regs

run :: (a -> Maybe a) -> a -> [a]
run f x = x : case f x of
  Just y  -> run f y
  Nothing -> []

willMult :: State -> Bool
willMult (S regs pc ins) =
  not (pc < 0 || pc > (length ins - 1))
    && ( case ins !! pc of
         (Mul _ _) -> True
         _         -> False
       )

solve1s = length . filter willMult . run stepState . startingState

regB ins = b where (Set 'b' (Lit b)) = head ins

solve1c ins = (regB ins - 2) ^ 2

solve2 ins = length $ filter (not . isPrime) [b, b + 17 .. b + 17000]
  where b = regB ins * 100 + 100000

prepareInput :: String -> [Ins]
prepareInput = map read . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1 Simulated: " ++ show (solve1s input)
  putStrLn $ "Part 1 Calculated: " ++ show (solve1c input)
  putStrLn $ "Part 2 Calculated: " ++ show (solve2 input)
