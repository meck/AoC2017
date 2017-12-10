module Main where

import Data.Char
import Data.List
import Data.List.Unique
import Data.Maybe
import Control.Applicative


data Program = Program { name :: String
                       , weight ::Int
                       , leaves :: [Program] }
                       deriving (Show)

instance Eq Program where
  a == b = totWeight a == totWeight b

totWeight :: Program -> Int
totWeight p = if null $ leaves p
  then weight p
  else foldr ((+) . totWeight) (weight p) (leaves p)

isBal :: Program -> Bool
isBal (Program _ _ []) = True
isBal (Program _ _ (n:ns)) =
  foldr (\m acc -> (totWeight m == totWeight n) && acc) True ns

buildTree :: [(Program, [String])] -> String -> Program
buildTree list root = (fst res) { leaves = map (buildTree list) (snd res) }
  where res = fromJust $ find (\(p, _) -> name p == root) list

findRootName :: [(Program, [String])] -> String
findRootName lst = go (name $ fst $ head lst) $ filter (not . null . snd) lst
 where
  go str xs = case find (\(p1, ch) -> str `elem` ch) xs of
    Nothing -> str
    Just a  -> go (name $ fst a) xs

findUnbalNode :: Program -> Maybe Program
findUnbalNode n
  | isBal n   = Nothing
  | all isBal (leaves n) && not (isBal n) = Just n
  | otherwise = head $ filter isJust $ map findUnbalNode (leaves n)

solve2 p = liftA3 (\x y z -> x - y + z)
                  (totWeight <$> target)
                  (totWeight <$> offender)
                  (weight <$> offender)
 where
  offender = listToMaybe $ (\(_, _, a) -> a) $ complex $ leaves p
  target   = listToMaybe $ (\(_, a, _) -> a) $ complex $ leaves p

prepareInput =
  map (prepareInput' . words . filter (\c -> isAlphaNum c || isSeparator c))
    . lines

prepareInput' (n:w:xs) = (Program n (read w) [], xs)


main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ findRootName input
  putStrLn $ "Part 2: " ++ show
    ( fromJust
    $ solve2
    $ fromJust
    $ findUnbalNode
    $ buildTree input
    $ findRootName input
    )
