module Main where

import Data.Char
import Data.Map.Strict
       (Map, (!?), delete, fromList, insert, member, notMember)

data Dir
  = N
  | E
  | S
  | W
  deriving Enum

turnLeft :: Dir -> Dir
turnLeft N = W
turnLeft d = pred d

turnRight :: Dir -> Dir
turnRight W = N
turnRight d = succ d

type Pos = (Int, Int)

newPos :: Pos -> Dir -> Pos
newPos (x, y) d = case d of
  N -> (x, y + 1)
  E -> (x + 1, y)
  S -> (x, y - 1)
  W -> (x - 1, y)

data NodeState
  = Weak
  | Infected
  | Flagged

data State = State
  { pos :: Pos
  , dir :: Dir
  , nodes :: Map Pos NodeState
  }

startingState = State (0, 0) N

makeGrid i = fromList
  [ ((x, y), Infected) | (y, row) <- idy i, (x, tile) <- idx row, tile == '#' ]
 where
  idx = zip [(negate mx) ..]
  idy = zip [my, (my - 1) ..]
  mx  = length (head i) `div` 2
  my  = length i `div` 2

-- Part 1
stepStateA :: State -> State
stepStateA s = State (newPos (pos s) newDir) newDir newNodeState
 where
  newDir =
    if pos s `member` nodes s then turnRight (dir s) else turnLeft (dir s)
  newNodeState =
    let p = pos s
        n = nodes s
    in  if p `member` n then delete p n else insert p Infected n

willInfectA :: State -> Bool
willInfectA s = pos s `notMember` nodes s

-- Part 2
stepStateB :: State -> State
stepStateB s = State (newPos (pos s) newDir) newDir newNodeState
 where
  newDir = case nodes s !? pos s of
    Nothing       -> turnLeft $ dir s
    Just Weak     -> dir s
    Just Infected -> turnRight $ dir s
    Just Flagged  -> turnRight $ turnRight $ dir s
  newNodeState =
    let p = pos s
        n = nodes s
    in  case nodes s !? pos s of
          Nothing       -> insert p Weak n
          Just Weak     -> insert p Infected n
          Just Infected -> insert p Flagged n
          Just Flagged  -> delete p n

willInfectB :: State -> Bool
willInfectB s = case nodes s !? pos s of
  (Just Weak) -> True
  _           -> False

solve1 =
  length . filter willInfectA . take 10000 . iterate stepStateA . startingState

solve2 =
  length
    . filter willInfectB
    . take 10000000
    . iterate stepStateB
    . startingState

prepareInput = makeGrid . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
