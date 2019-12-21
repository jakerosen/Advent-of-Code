module Main where

import Data.List
import Data.Maybe
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Control.Lens
import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Vector.Unboxed (Vector, (//), (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.Primitive
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Coerce
import Data.Semigroup
import Data.Semigroup.Union

main :: IO ()
main = day3p2

day1p1 :: IO ()
day1p1 = do
  masses :: [Int] <- map tToI <$> readLines "data/day1-1"
  let
    ans = sum $ map fuel masses
  print ans

day1p2 :: IO ()
day1p2 = do
  masses :: [Int] <- map tToI <$> readLines "data/day1-1"
  let
    ans = sum $ map fuel2 masses
  print ans

fuel :: Int -> Int
fuel m = if x > 0 then x else 0
  where
    x = m `div` 3 - 2

fuel2 :: Int -> Int
fuel2 m = if x > 0
  then x + fuel2 x
  else x
  where
    x = fuel m


readLines :: FilePath -> IO [Text]
readLines file = T.lines <$> T.readFile file

tToI :: Integral a => Text -> a
tToI t = (fromRight undefined $ T.decimal t) ^. _1

day2p1 :: IO ()
day2p1 = do
  contents :: Text <- T.stripEnd <$> T.readFile "data/day2-1"
  let
    vals :: [Int]
    vals = tToI <$> T.splitOn "," contents
  let
    init :: Vector Int
    init = V.fromList vals

  mem :: Memory <- V.thaw init
  result :: Vector Int <- runIntCode 0 mem >>= V.freeze
  print result

day2p2 :: IO ()
day2p2 = do
  contents :: Text <- T.stripEnd <$> T.readFile "data/day2-1"
  let
    vals :: [Int]
    vals = tToI <$> T.splitOn "," contents

    init :: Vector Int
    init = V.fromList vals

    is :: [(Int, Int)]
    is = [(x, y) | x <- [0..99], y <- [0..99]]

    f :: (Int, Int) -> Vector Int
    f (a, b) = init // [(1, a), (2, b)]

    vs :: [Vector Int]
    vs = map f is

  mem :: [Memory] <- traverse V.thaw vs
  result :: [Vector Int] <- traverse (runIntCode 0) mem >>= traverse V.freeze
  let
    vec :: Vector Int
    vec = fromJust $ find (\v -> V.head v == 19690720) result

    x :: Int
    x = vec ! 1

    y :: Int
    y = vec ! 2
  print (x * 100 + y)

type Memory = IOVector Int

-- This function should recursively parse the whole memory, computing the
-- final result
runIntCode
  :: Int         -- index to read
  -> Memory      -- initial state
  -> IO Memory      -- resulting state
runIntCode i v = do
  cmd <- V.read v i
  case cmd of
    1 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- V.read v xi
      y <- V.read v yi
      z <- V.read v zi
      V.write v z (x + y)
      runIntCode (i + 4) v
    2 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- V.read v xi
      y <- V.read v yi
      z <- V.read v zi
      V.write v z (x * y)
      runIntCode (i + 4) v
    99 -> pure v
    _ -> error "Input was bad or machine failed"

day3p1 :: IO ()
day3p1 = do
  [input1 :: [Text], input2 :: [Text]] <-
    map (T.splitOn ",") <$> readLines "data/day3-1"
  let
    -- input1 :: [Text]
    -- input1 = ["R8", "U5", "L5", "D3"]

    -- input2 :: [Text]
    -- input2 = ["U7", "R6", "D4", "L4"]

    paths1 :: [Path]
    paths1 = map parsePath input1

    paths2 :: [Path]
    paths2 = map parsePath input2

    wire1 :: HashSet Coord
    wire1 = wireCoords (0,0) paths1

    wire2 :: HashSet Coord
    wire2 = wireCoords (0,0) paths2

    intersections :: HashSet Coord
    intersections = HS.intersection wire1 wire2

    ans :: Int
    ans = minimum $ map l1FromOrigin (HS.toList intersections)

  print ans

data Path = PathUp Int | PathDown Int | PathLeft Int | PathRight Int
  deriving Show

type Coord = (Int, Int)

l1FromOrigin :: Coord -> Int
l1FromOrigin (x, y) = abs x + abs y

parsePath :: Text -> Path
parsePath t = case T.head t of
  'U' -> PathUp (tToI (T.tail t))
  'D' -> PathDown (tToI (T.tail t))
  'L' -> PathLeft (tToI (T.tail t))
  'R' -> PathRight (tToI (T.tail t))
  _ -> undefined -- shouldn't happen

runPath :: Coord -> Path -> [Coord]
runPath (a, b) p = case p of
  PathUp x -> zip (repeat a) (map (b+) [x, x-1 .. 1])
  PathDown x -> zip (repeat a) (map (b-) [x, x-1 .. 1])
  PathLeft x -> zip (map (a-) [x, x-1 .. 1]) (repeat b)
  PathRight x -> zip (map (a+) [x, x-1 .. 1]) (repeat b)

wireCoords :: Coord -> [Path] -> HashSet Coord
wireCoords start paths = HS.delete start coords -- start doesn't count
  where
    f :: [Coord] -> Path ->  [Coord]
    f [] _ = []
    f (x:xs) p = runPath x p

    coords :: HashSet Coord
    -- coords = HS.fromList $ concat (scanr f [start] (reverse paths))
    coords = HS.fromList $ concat (scanl f [start] paths)

data WireCoord = WireCoord
  { wireCoord :: Coord
  , wireSteps :: Int
  } deriving (Show)

instance Eq WireCoord where
  (WireCoord a _) == (WireCoord b _) = a == b

instance Hashable WireCoord where
  hash a = hash (wireCoord a)
  hashWithSalt x a = hashWithSalt x (wireCoord a)

extractWireCoord :: WireCoord -> (Coord, Int)
extractWireCoord (WireCoord x y) = (x, y)

runPathP2 :: WireCoord -> Path -> [WireCoord]
runPathP2 (WireCoord (a, b) step) p = case p of
  PathUp x -> zipWith3 (\u v w -> WireCoord (u, v) w)
    (repeat a)
    (map (b+) [x, x-1 .. 1])
    [step + x, step + x - 1..]
  PathDown x -> zipWith3 (\u v w -> WireCoord (u, v) w)
    (repeat a)
    (map (b-) [x, x-1 .. 1])
    [step + x, step + x - 1..]
  PathLeft x -> zipWith3 (\u v w -> WireCoord (u, v) w)
    (map (a-) [x, x-1 .. 1])
    (repeat b)
    [step + x, step + x - 1..]
  PathRight x -> zipWith3 (\u v w -> WireCoord (u, v) w)
    (map (a+) [x, x-1 .. 1])
    (repeat b)
    [step + x, step + x - 1..]

wireCoordsP2 :: Coord -> [Path] -> Map Coord Int
wireCoordsP2 start paths = M.delete start coords -- start doesn't count
  where
    f :: [WireCoord] -> Path ->  [WireCoord]
    f [] _ = []
    f (x:xs) p = runPathP2 x p

    g :: WireCoord -> WireCoord -> Bool
    g (WireCoord a x) (WireCoord b y) = a == b && x >= y

    coordList :: [(Coord, Int)]
    coordList =
      map extractWireCoord (concat (scanl f [WireCoord start 0] paths))

    foo :: (Coord, Int) -> Map Coord Int
    foo = uncurry M.singleton

    coords :: Map Coord Int
    coords = coerce $ foldMap
      (coerce foo :: (Coord, Int) -> (UnionWith (Map Coord)) (Min Int))
      coordList

day3p2 :: IO ()
day3p2 = do
  [input1 :: [Text], input2 :: [Text]] <-
    map (T.splitOn ",") <$> readLines "data/day3-1"
  let
    -- input1 :: [Text]
    -- input1 = ["R8", "U5", "L5", "D3"]

    -- input2 :: [Text]
    -- input2 = ["U7", "R6", "D4", "L4"]

    paths1 :: [Path]
    paths1 = map parsePath input1

    paths2 :: [Path]
    paths2 = map parsePath input2

    wire1 :: Map Coord Int
    wire1 = wireCoordsP2 (0,0) paths1

    wire2 :: Map Coord Int
    wire2 = wireCoordsP2 (0,0) paths2

    intersections :: Map Coord Int
    intersections = M.intersectionWith (+) wire1 wire2

    intersectList :: [(Coord, Int)]
    intersectList = M.toList intersections

    minIntersect :: (Coord, Int)
    minIntersect = minimumBy (\(_, a) (_, b) -> compare a b) intersectList

    ans :: Int
    ans = snd minIntersect

  print ans
