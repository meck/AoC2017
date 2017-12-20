module Main where

import Data.Matrix
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad

type Cord = (Int,Int)

data Tile = Hor | Ver | Corn | Let Char | Empty deriving Eq

charToTile :: Char -> Tile
charToTile c = case c of
  ' ' -> Empty
  '-' -> Hor
  '|' -> Ver
  '+' -> Corn
  _   -> Let c

type Grid = Matrix Tile

data Dir = N | E | S | W
data Pos = Pos Cord Dir

changeDir :: Grid -> Pos -> Maybe Dir
changeDir g (Pos c d) = case d of
  N -> go [W, E]
  E -> go [S, N]
  S -> go [W, E]
  W -> go [S, N]
 where
  go =
    listToMaybe
      . fmap fst
      . filter ((Empty/=) . snd)
      . fmap (fmap fromJust)
      . filter (isJust . snd)
      . (fmap (flip (uncurry safeGet) g)<$>)
      . ap zip (goDir c<$>)


goDir :: Cord -> Dir -> Cord
goDir (r, c) N = (r - 1, c)
goDir (r, c) E = (r, c + 1)
goDir (r, c) S = (r + 1, c)
goDir (r, c) W = (r, c - 1)

walk :: Grid -> Pos -> (String, Int)
walk g = go "" 0
 where
  go str i p = case step p of
    Nothing            -> (str, succ i)
    Just (p1, Nothing) -> go str (succ i) p1
    Just (p1, Just c ) -> go (str ++ [c]) (succ i) p1
  step (Pos c d) =
    let next = goDir c d
    in  case g ! next of
          Empty   -> Nothing
          Hor     -> Just (Pos next d, Nothing)
          Ver     -> Just (Pos next d, Nothing)
          (Let c) -> Just (Pos next d, Just c)
          Corn    -> case changeDir g (Pos next d) of
            (Just nd) -> Just (Pos next nd, Nothing)
            Nothing   -> Nothing


findStart :: Grid -> Pos
findStart g = Pos (1, (+1) $ fromJust $ V.elemIndex Ver $ getRow 1 g) S

solve1 g = fst $ walk g (findStart g)
solve2 g = snd $ walk g (findStart g)

prepareInput :: String -> Grid
prepareInput = fromLists . fmap (charToTile<$>) . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
