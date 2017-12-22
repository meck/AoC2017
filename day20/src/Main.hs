module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Tuple.HT
import Data.Map.Lazy (Map, elems, fromList, toList, assocs, filterWithKey, size )
import qualified Data.Map.Lazy  as M
import Control.Monad


data Particle = Par
  { pos :: (Int, Int, Int)
  , vel :: (Int, Int, Int)
  , acc :: (Int, Int, Int)
  } deriving (Show)

instance Read Particle where
  readsPrec _ str =
    let [px, py, pz, vx, vy, vz, ax, ay, az] =
          splitOn "," $ filter (\c -> isDigit c || '-' == c || ',' == c) str
    in [ ( Par
             (read px, read py, read pz)
             (read vx, read vy, read vz)
             (read ax, read ay, read az)
         , [])
       ]

stepPar :: Particle -> Particle
stepPar p = Par pnew vnew (acc p)
 where
  vnew = liftTrip (+) (vel p) (acc p)
  pnew = liftTrip (+) vnew (pos p)

distPair :: Particle -> Particle -> Int
distPair p0 p1 = sumTrip $ mapTrip' abs $ liftTrip (-) (pos p0) (pos p1)

tot f p = sumTrip $ mapTrip' abs $ f p
mapTrip' = mapTriple . triple
liftTrip = (mapTriple.) . mapTrip'
sumTrip (x, y, z) = x + y + z

type State = Map Int Particle

allSeparating :: State -> Bool
allSeparating s = and $ zipWith (>) (distances (stepPar <$> s)) (distances s)
 where
  distances = fmap (uncurry distPair) . getPairs
  getPairs m = [ (x, y) | (x:ys) <- tails (elems m), y <- ys ]

allOutBound :: State -> Bool
allOutBound s = and $ zipWith (>) (tot pos <$> (stepPar <$> l)) (tot pos <$> l)
  where l = snd <$> toList s

closestToOrig :: Map a Particle -> (a, Int)
closestToOrig = minimumBy ((.snd) . compare . snd) . toList . M.map (tot pos)

filterLowestAcc :: State -> State
filterLowestAcc s = filterWithKey (\k _ -> k `elem` lowest) s
 where
  lowest =
    fmap fst
      $ head
      $ groupBy ((.snd) . (==) . snd)
      $ sortBy ((.snd) . compare . snd)
      $ toList
      $ M.map (tot acc) s

getSlowest :: State -> Int
getSlowest s =
  fst $ minimumBy ((.snd) . compare . snd) $ toList $ M.map (tot vel) s

filterCol :: State -> State
filterCol s = filterWithKey (const . flip notElem ks) s
 where
  ks = concat $ isColiding <$> getPairs s
  isColiding ((k1, p1), (k2, p2)) = if pos p1 == pos p2 then [k1, k2] else []
  getPairs m = [ (x, y) | (x:ys) <- tails (assocs m), y <- ys ]

run :: (State -> State) -> State -> State
run filt = until (liftM2 (&&) allSeparating allOutBound) (filt . M.map stepPar)

solve1 = getSlowest . run id . filterLowestAcc
solve2 = size . run filterCol

prepareInput :: String -> State
prepareInput = fromList . zip [0 ..] . fmap read . lines

main :: IO ()
main = do
  input <- prepareInput <$> readFile "Input.txt"
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
