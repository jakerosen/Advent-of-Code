module Main where
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import Control.Lens
import Control.Monad.Primitive
import Data.Coerce
import Data.Either
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import Data.Semigroup.Union
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Vector.Unboxed ((!), (//), Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as V
import Debug.Trace
import Data.Sequence (Seq ((:<|)), (|>), (><))
import qualified Data.Sequence as Seq
import Control.Algebra
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Carrier.Reader
import Control.Carrier.Error.Either
import Control.Monad.IO.Class

import IntCode

main :: IO ()
main = day7p2

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
tToI t = (fromRight undefined $ T.signed T.decimal t) ^. _1

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
  result :: Vector Int <- runIntCodeLegacy 0 mem >>= V.freeze
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
  result :: [Vector Int] <- traverse (runIntCodeLegacy 0) mem >>= traverse V.freeze
  let
    vec :: Vector Int
    vec = fromJust $ find (\v -> V.head v == 19690720) result

    x :: Int
    x = vec ! 1

    y :: Int
    y = vec ! 2
  print (x * 100 + y)

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

day4p1 :: IO ()
day4p1 = do
  let
    lo :: Int
    lo = 278384

    hi :: Int
    hi = 824795

  -- print (day4PassCriteria 221)
  -- print (day4PassCriteria 223)
  -- print (day4PassCriteria 111111)
  -- print (day4PassCriteria 223450)
  -- print (day4PassCriteria 123789)
  -- print (day4PassCriteria lo)
  -- print (day4PassCriteria hi)
  print (length $ filter day4PassCriteria [lo..hi])

day4PassCriteria :: Int -> Bool
day4PassCriteria x = if n == 0 then False else u && v
  where
    n :: Int
    r :: Int
    (n, r) = x `divMod` 10
    (u, v) = loop n r False

    loop :: Int -> Int -> Bool -> (Bool, Bool)
    loop 0 _ c = (c, True)
    loop x y c = if y >= r then loop n r c' else (c', False)
      where
        n :: Int
        r :: Int
        (n, r) = x `divMod` 10

        c' :: Bool
        c' = c || y == r

day4p2 :: IO ()
day4p2 = do
  let
    lo :: Int
    lo = 278384

    hi :: Int
    hi = 824795

  -- print (day4PassCriteriaP2 344)
  -- print (day4PassCriteriaP2 44)
  -- print (day4PassCriteriaP2 3444)
  -- print (day4PassCriteria 001111)
  -- print (day4PassCriteriaP2 001111)
  -- print (day4PassCriteriaP2 112233)
  -- print (day4PassCriteriaP2 123444)
  -- print (day4PassCriteriaP2 111122)
  -- print (day4PassCriteria lo)
  -- print (day4PassCriteria hi)
  print (length $ filter day4PassCriteriaP2 [lo..hi])

day4PassCriteriaP2 :: Int -> Bool
day4PassCriteriaP2 x = day4PassCriteria x && c
  where
    c :: Bool
    c = elem 2 $ map length (group (show x))

day5p1 :: IO ()
day5p1 = do
  contents :: Text <- T.stripEnd <$> T.readFile "data/day5-1"
  let
    vals :: [Int]
    vals = tToI <$> T.splitOn "," contents

    initial :: Vector Int
    initial = V.fromList vals

  mem :: Memory <- V.thaw initial
  result :: Vector Int <- runIntCodeLegacy 0 mem >>= V.freeze
  print result

day5p2 :: IO ()
day5p2 = day5p1

day6p1 :: IO ()
day6p1 = do
  orbits :: [(Text, Text)] <- map parseOrbit <$> readLines "data/day6-1"
  let
    orbitCount :: Map Text Int
    orbitCount = countOrbits orbits

    ans :: Int
    ans = sum (M.elems orbitCount)
  print ans
  -- print orbitCount

parseOrbit :: Text -> (Text, Text)
parseOrbit t =
  let [a, b] = T.splitOn ")" t
  in (a, b)

countOrbits
  :: [(Text, Text)]
  -> Map Text Int
countOrbits orbits = go orbits M.empty M.empty
  where
    go
      :: [(Text, Text)]
      -> Map Text Int
      -> Map Text (Set Text)
      -> Map Text Int
    go [] orbitCount _ = orbitCount
    go ((x,y):orbits) orbitCount orbitedBy =
      let
        yCount :: Int
        yCount = case M.lookup x orbitCount of
          Nothing -> 1
          Just xCount -> 1 + xCount

        orbitCount' :: Map Text Int
        orbitCount' = M.insert
          y
          yCount
          orbitCount

        orbitCount'' :: Map Text Int
        orbitCount'' = case M.lookup y orbitedBy of
          Nothing -> orbitCount'
          Just planets ->
            updateAll
              yCount
              (S.toList planets)
              orbitedBy
              orbitCount'

        -- y orbits x
        orbitedBy' :: Map Text (Set Text)
        orbitedBy' = case M.lookup x orbitedBy of
          Nothing -> M.insert x (S.singleton y) orbitedBy
          Just ps -> M.insert x (S.insert y ps) orbitedBy

        in go orbits orbitCount'' orbitedBy'

    update
      :: Int
      -> Text
      -> Map Text (Set Text)
      -> Map Text Int
      -> Map Text Int
    update bump planet orbitedBy orbitCount =
      let
        orbitCount' :: Map Text Int
        orbitCount' = M.alter (Just . maybe bump (+bump)) planet orbitCount
        -- case M.lookup planet orbitCount of
        --   Nothing -> M.insert planet bump orbitCount
        --   Just count -> M.insert planet (count + bump) orbitCount

        planets :: [Text]
        planets = case M.lookup planet orbitedBy of
          Nothing -> []
          Just ps -> S.toList ps
      in
        updateAll bump planets orbitedBy orbitCount'

    updateAll
      :: Int
      -> [Text]
      -> Map Text (Set Text)
      -> Map Text Int
      -> Map Text Int
    updateAll bump planets orbitedBy =
      appEndo $ foldMap
        (coerce f :: Text -> Endo (Map Text Int))
        planets
      where
        f :: Text -> Map Text Int -> Map Text Int
        f p = update bump p orbitedBy

day6p2 :: IO ()
day6p2 = do
  orbits :: [(Text, Text)] <- map parseOrbit <$> readLines "data/day6-1"
  let
    graph :: Graph Text
    graph = makeGraph orbits

  -- undefined
  print (bfs "YOU" "SAN" graph)
  -- print (graph)
  -- print (M.lookup "ZZC" graph)
  -- print (M.lookup "YOU" graph)
  -- print (M.lookup "SAN" graph)

type Graph a = Map a (Set a)

addEdge :: Ord a => Graph a -> (a, a) -> Graph a
addEdge m (a, b) =
  let
    put :: Ord a => a -> Maybe (Set a) -> Maybe (Set a)
    put x s = Just $ maybe (S.singleton x) (S.insert x) s

    m' = M.alter (put b) a m
  in     M.alter (put a) b m'

makeGraph :: Ord a => [(a, a)] -> Graph a
makeGraph = foldl' addEdge M.empty

bfs :: forall a. (Ord a, Show a) => a -> a -> Graph a -> Maybe Int
bfs src end graph = go (S.empty) (Seq.fromList [(src, 0)])
  where
    go :: Set a -> Seq (a, Int) -> Maybe Int
    go _ Seq.Empty = Nothing
    go visited ((x, n) :<| queue) = do
      let
        visited' :: Set a
        visited' = S.insert x visited

        neighbors :: Set a
        neighbors = case M.lookup x graph of
          Nothing -> S.empty
          Just ps -> ps

        neighbors' :: [a]
        neighbors' = S.toList $ neighbors `S.difference` visited

        toAdd :: Seq (a, Int)
        toAdd = Seq.fromList $ zip neighbors' (repeat (n+1))

        queue' :: Seq (a, Int)
        queue' = queue >< toAdd

      if x == end then Just n else go visited' queue'

makeAlgebraGraph :: [(Text, Text)] -> AdjacencyMap Text
makeAlgebraGraph = AM.edges

day7p1 :: IO ()
day7p1 = do
  list :: [Int] <- map tToI <$> T.splitOn "," <$> T.readFile "data/day7-1"
  let
    -- initialValues = V.fromList
    --   [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
    initialValues = V.fromList list
  mem :: Memory <- V.thaw initialValues
  -- res <- runIntCodeLegacy2 [4, 0] 0 mem >>= V.freeze
  -- res <- runIntCodeLegacy2 [3, 4] 0 mem >>= V.freeze
  -- res <- runIntCodeLegacy2 [2, 43] 0 mem >>= V.freeze
  -- res <- runIntCodeLegacy2 [1, 432] 0 mem >>= V.freeze
  -- output :: Either String [Int]
  --   <- runIntCodeLegacy3 0
  --     & runReader mem
  --     & execWriter
  --     & evalState [0 :: Int, 4321]
  --     & runError

  output2 :: Either String Int
    <- loop [4,3,2,1,0] (Seq.singleton 0) initialValues  & runError

  let
    -- output3 :: [IO (Either String Int)]
  output3 :: [Either String Int] <-
    traverse
      (\phase -> loop phase (Seq.singleton 0) initialValues & runError)
      allCombs
  let
    output4 :: Int
    output4 = maximum $ map (fromRight 0) output3

  -- ErrorC E (ReaderC Mem (WriterC W (StateC S IO))) A
  --          (ReaderC Mem (WriterC W (StateC S IO))) (Either E A)
  --                        WriterC W (StateC S IO) (Either E A)
  --                                  (StateC S IO) (W, Either E A)
  --                                  IO (S, (W, Either E A))

  --         (ReaderC Mem (WriterC W (StateC S (ErrorC E IO))) A
  --                      (WriterC W (StateC S (ErrorC E IO))) A
  --                                 (StateC S (ErrorC E IO))) (W, A)
  --                                           (ErrorC E IO)   (S, (W, A))

  --                                                 IO    Either E (S, (W, A))
  --                                  IO  S
    -- [0, 4321] [] 0 mem >>= V.freeze

  print output4
  -- print initialValues
  where
    loop
      :: (Has (Error String) sig m, Effect sig, MonadIO m)
      => [Int] -- phase
      -> Seq Int -- input
      -> Vector Int -- values
      -> m Int
    loop [] Seq.Empty _ = throwError @String "wut"
    loop [] (x :<| _) _ = pure x
    loop (x:xs) input vals = do
      mem :: Memory <- liftIO $ V.thaw vals
      output :: Seq Int <- runIntCode 0
        & runReader mem
        & execState (x :<| input)
      loop xs output vals

    allCombs = permutations [0..4]

-- TODO: I realize I need some sort of blocking queue or something to use
-- for reading input and writing output to. I'll do that later, I suppose.
day7p2 :: IO ()
day7p2 = do
  list :: [Int] <- map tToI <$> T.splitOn "," <$> T.readFile "data/day7-1"
  let
    -- initialValues :: Vector Int
    -- initialValues = V.fromList
    --   [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
    --     27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
    initialValues = V.fromList list
  output :: Either String Int
    <- loop [9,8,7,6,5] (Seq.singleton 0) initialValues & runError
  output3 :: [Either String Int] <-
    traverse
      (\phase -> loop phase (Seq.singleton 0) initialValues & runError)
      allCombs
  let
    output4 :: Int
    output4 = maximum $ map (fromRight 0) output3

  print output4
  -- undefined

  where
    loop
      :: (Has (Error String) sig m, Effect sig, MonadIO m)
      => [Int] -- phase
      -> Seq Int -- input
      -> Vector Int -- values
      -> m Int
    loop [] (Seq.Empty) _ = throwError @String "wut"
    loop [] (x :<| _) _ = pure x
    loop (x:xs) input vals = do
      mem :: Memory <- liftIO $ V.thaw vals
      output :: Seq Int <- runIntCode 0
        & runReader mem
        & execState (x :<| input)
      loop xs output vals

    allCombs = permutations [5..9]
