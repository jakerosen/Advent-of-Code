module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Text.Read as T
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.Either (fromRight)
import Data.Foldable
-- import Data.Array (Array)
-- import qualified Data.Array as A
import Data.Vector (Vector)
import Data.Vector ((!))
import qualified Data.Vector as V
-- import Data.Vector.Mutable (MVector)
-- import qualified Data.Vector.Mutable as MV
-- import Debug.Trace
-- import Data.List.Split
import Data.List
import Data.Map.Lazy (Map)
-- import Data.Map.Lazy ((!?))
import qualified Data.Map.Lazy as M
-- import Data.Char
-- import Data.Tuple (swap)
import Data.Set (Set)
-- import qualified Data.Set as S
import qualified Data.Set as Set
import Data.Maybe
-- import qualified Control.Monad.State.Lazy as State
-- import Control.Applicative
import Control.Lens hiding (children)
import Data.Generics.Labels ()
-- import GHC.Generics (Generic)
import Text.Printf
import Control.Arrow ((>>>))
-- import Control.Algebra
-- import Control.Carrier.State.Strict
-- import Data.Sequence (Seq, Seq ((:<|), (:|>)))
-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
-- import Data.Word
-- import Data.Bits
-- import Numeric (readInt)
-- import Control.Monad.ST
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Applicative.Lift
-- import Control.Monad.ListM
import Data.Coerce
-- import Text.Parsec
import Text.ParserCombinators.ReadP

import Helpers

main :: IO ()
main = d20p2



-- Oog.  This one is gonna take a bit to think out.
--
-- Consider this simple example
-- 0: 4 1 5
-- 1: 2 3 | 3 2
-- 2: 4 4 | 5 5
-- 3: 4 5 | 5 4
-- 4: "a"
-- 5: "b"
--
-- start with rule 0
-- 0: 4 1 5
-- replace each with their definition until all are concrete
-- 0: a (2 3 | 3 2) b
-- 0: a ((4 4 | 5 5) (4 5 | 5 4) | (4 5 | 5 4) (4 4 | 5 5)) b
-- 0: a ((a a | b b) (a b | b a) | (a b | b a) (a a | b b)) b
--
-- Now all are concrete but we need to reduce
-- I think this is cartesian product for each dude
-- So, like (a a | b b) (a b | b a) is the cartesian product of
-- [aa, bb] [ab, ba]
-- The cartesian product of that resolves to
-- [(aa, ab), (aa, ba), (bb, ab), (bb, ba)]
-- So this appears to be correct
--
-- Each | essentially resolves to a list concatenation ++ and a space between
-- list entries resolves to a cartesian product (++) between the two lists.
--
-- Let's use this logic to resolve the example
-- 0: a ((a a | b b) (a b | b a) | (a b | b a) (a a | b b)) b
-- We basically have 3 lists here:
-- a
-- ((a a | b b) (a b | b a) | (a b | b a) (a a | b b))
-- b
--
-- The middle list is the most complex, so let's resolve that first.
-- ((a a | b b) (a b | b a) | (a b | b a) (a a | b b))
-- Strip unnecessary spacing within list entries
-- ((aa | bb) (ab | ba) | (ab | ba) (aa | bb))
-- Resolve | = list concatenation ++
-- ([aa, bb] [ab, ba] | [ab, ba] [aa, bb])
-- Resolve space between lists = cartesian product (++)
-- (cartProd [aa, bb] [ab, ba] ++ cartProd [ab, ba] [aa, bb])
-- [aaab, aaba, bbab, bbba] ++ [abaa, abbb, baaa, babb]
-- [aaab, aaba, bbab, bbba, abaa, abbb, baaa, babb]
-- This is what the middle list has resolved to, but we still have 3 lists
--
-- a
-- [aaab, aaba, bbab, bbba, abaa, abbb, baaa, babb]
-- b
--
-- These lists follow the same rules.  There was a space between them, so
-- cartProd them.
-- cartProd a [aaab, aaba, bbab, bbba, abaa, abbb, baaa, babb]
-- [aaaab, aaaba, abbab, abbba, aabaa, aabbb, abaaa, ababb]
-- cartProd [aaaab, aaaba, abbab, abbba, aabaa, aabbb, abaaa, ababb] b
-- [aaaabb, aaabab, abbabb, abbbab, aabaab, aabbbb, abaaab, ababbb]
-- And that is the final result.  Once we have the final result, checking
-- an "image" is easy.  Simply "is it in this list?"
--
-- There's another possible execution order that might work.  Rather than
-- trying to resolve each index down to its base form immediately, I can try
-- combining them during each step, so I'll only ever have a list of indices
-- until they are resolved to the most basic indices.
--
-- Let's keep them in index form for now, but we note that 4 and 5 are "base
-- indices", so we basically want to combine and reduce until we get to all
-- 4s and 5s.  Replacing 4s and 5s with their definition is ez.  This is
-- merely to make the types easier.  I should be working with simply [[Int]]
-- the whole time.
--
-- start with rule 0
-- 0: 4 1 5
-- replace with def
-- 0: 4 (2 3 | 3 2) 5
-- now reduce
-- 0: 4 2 3 5 | 4 3 2 5
-- replace with def
-- 0: 4 (4 4 | 5 5) (4 5 | 5 4) 5 | 4 (4 5 | 5 4) (4 4 | 5 5) 5
-- now reduce
-- 444455
-- 444545
-- 455455
-- 455545
-- 445445
-- 445555
-- 454445
-- 454555
-- And of course this would trivially turn into the as and bs.  I am unsure if
-- this is more, less, or the same amount of computation, but it would almost
-- certainly be easier to code in terms of types, since I would only have
-- to worry about having a [[Int]].
d19p1 :: IO ()
d19p1 = do
  m <- T.readFile "data/d19p1.txt"
    <&> T.lines

  let
    (rules, _:images) = splitAt 135 m

    foo :: [(Int, [[Int]])]
    foo = sortBy (\(a,_) (b,_) -> compare a b) $ rules
      & map (T.splitOn ": "
        >>> splitAt 1
        >>> both %~ head
        >>> _1 %~ tToI
        >>> _2 %~ (T.splitOn " | "
          >>> map (T.split (==' ') >>> map tToI)
          )
        )

    foo2 :: Map Int (Set Text)
    foo2 = foldr step M.empty foo

    step :: (Int, [[Int]]) -> Map Int (Set Text) -> Map Int (Set Text)
    step (rule, definition) = M.insert rule (case rule of
      52 -> Set.singleton "a"
      72 -> Set.singleton "b"
      _ -> Set.unions (map (unCPSet . foldMap1 (CPSet . bar)) definition)
      )

    bar :: Int -> Set Text
    bar def = foo2 M.! def

    -- (_, rs) = unzip foo

    -- rv :: Vector [[Int]]
    -- rv = V.fromList rs

    ans = length $ filter (\x -> Set.member x (bar 0)) images

  -- print $ head images
  -- for_ (V.indexed rv) print
  print $ length $ bar 0
  print ans

foldMap1 :: Semigroup m => (a -> m) -> [a] -> m
foldMap1 f xs = foldl1' (<>) (map f xs)

newtype CPSet a = CPSet {unCPSet :: Set a}

instance (Ord a, Semigroup a) => Semigroup (CPSet a) where
  (<>) :: CPSet a -> CPSet a -> CPSet a
  (<>) = coerce oink
    where
      oink :: Set a -> Set a -> Set a
      oink xs ys = Set.map (uncurry (<>)) (Set.cartesianProduct xs ys)

d19p2 :: IO ()
d19p2 = do
  m <- T.readFile "data/d19p2.txt"
    <&> T.lines

  let
    (rules, _:images) = splitAt 135 m

    foo :: [(Int, [[Int]])]
    foo = sortBy (\(a,_) (b,_) -> compare a b) $ rules
      & map (T.splitOn ": "
        >>> splitAt 1
        >>> both %~ head
        >>> _1 %~ tToI
        >>> _2 %~ (T.splitOn " | "
          >>> map (T.split (==' ') >>> map (tToIMaybe >>> fromMaybe (-1)))
          )
        )

    foo2 :: Vector (P ())
    foo2 = V.fromList $ map bar foo

    bar :: (Int, [[Int]]) -> P ()
    bar (i, def) =
      case i of
        52 -> char 'a' >> pure ()
        72 -> char 'b' >> pure ()
        _ -> asum (map (traverse_ (foo2 !)) def)

    baz :: P ()
    baz = foo2 ! 0


    ans = length $ filter (parserMatches baz) (map T.unpack images)

  print ans

parserMatches :: P a -> [Char] -> Bool
parserMatches parser str =
  any (\(_, xs) -> null xs) (readP_to_S parser str)

type P = ReadP

d20p1 :: IO ()
d20p1 = do
  m :: [(Int, [[Char]])] <- T.readFile "data/d20p1.txt"
    <&> (T.splitOn "\n\n" >>> init)
    <&> map (T.lines
      >>> splitAt 1
      >>> _1 %~ (head >>> T.drop 5 >>> tToI)
      >>> _2 %~ (\l ->
        [T.unpack (head l), T.unpack (last l), map T.head l, map T.last l])
      )

  let
    connections :: [(Int, [Maybe Int])]
    connections = map (\(tid, sides) ->
      (tid, map (\side -> fst <$> find (isConnected tid side) m) sides)) m

    isConnected :: Int -> [Char] -> (Int, [[Char]]) -> Bool
    isConnected tid side (otherID, otherSides) =
      tid /= otherID && any (\s -> s == side || reverse s == side) otherSides

    numConnections :: [(Int, Int)]
    numConnections = map (_2 %~ \ss -> 4 - length (catMaybes ss)) connections

    corners = filter (\(_,b) -> b == 2) numConnections

    ans = product (fst <$> corners)

  -- print m
  -- print $length m
  -- print $ head m
  -- print $ last m
  -- print $ length corners
  -- for_ connections print
  -- for_ numConnections print
  -- for_ corners print
  print ans

d20p2 :: IO ()
d20p2 = do
  m :: [(Int, [[Char]])] <- T.readFile "data/d20p1.txt"
    <&> (T.splitOn "\n\n" >>> init)
    <&> map (T.lines
      >>> splitAt 1
      >>> _1 %~ (head >>> T.drop 5 >>> tToI)
      >>> _2 %~ (\l ->
        [T.unpack (head l), T.unpack (last l), map T.head l, map T.last l])
      )

  cars :: [(Int, [[Char]])] <- T.readFile "data/d20p1.txt"
    <&> (T.splitOn "\n\n" >>> init)
    <&> map (T.lines
      >>> splitAt 1
      >>> _1 %~ (head >>> T.drop 5 >>> tToI)
      >>> _2 %~ map T.unpack
      )

  let
    -- connections :: [(Int, [Maybe Int])]
    -- connections = map (\(tid, sides) ->
    --   (tid, map (\side -> fst <$> find (isConnected tid side) m) sides)) m

    -- isConnected :: Int -> [Char] -> (Int, [[Char]]) -> Bool
    -- isConnected tid side (otherID, otherSides) =
    --   tid /= otherID && any (\s -> s == side || reverse s == side) otherSides

    -- numConnections :: [(Int, Int)]
    -- numConnections = map (_2 %~ \ss -> 4 - length (catMaybes ss)) connections

    -- corners = filter (\(_,b) -> b == 2) numConnections

    -- ans = product (fst <$> corners)

    withSides :: [(Int, [(D20Side, [Char])])]
    withSides = m <&> _2 %~ \case
      [up, down, left, right] ->
        [ (D20Up, up)
        , (D20Down, down)
        , (D20Left, left)
        , (D20Right, right)
        ]
      _ -> error "Invalid tile formation"

    -- charMap :: Map Int [[Char]]
    -- charMap = M.fromList m

    connMap :: Map Int [Maybe D20Connection]
    connMap = M.fromList $ map (\(tid, sides) ->
      (tid, map (\(s, cs) ->
        mkConn s cs <$> find (isConn2 tid cs) withSides) sides)
      )
      withSides

    imageMap :: Map Int D20Image
    imageMap = foldr (\(tid, chars) acc ->
      genImageMap tid chars connMap acc
      ) M.empty cars

    mkConn :: D20Side -> [Char] -> (Int, [(D20Side, [Char])]) -> D20Connection
    mkConn side cs (tid, sides) = D20Connection tid side (fromJust do
      (otherSide, _) <- find (\(_,s) -> s == cs || reverse s == cs) sides
      -- Just $ if cs == ocs -- will be equal to or reverse
      --   then otherSide
      --   -- else D20Flip otherSide
      --   else otherSide
      Just otherSide
      )

    isConn2 :: Int -> [Char] -> (Int, [(D20Side, [Char])]) -> Bool
    isConn2 tid cs (otherID, otherSides) =
      tid /= otherID && any (\(_,s) -> s == cs || reverse s == cs) otherSides

    -- start :: Int
    -- start = fst (fromJust (find (\(_, sides) ->
    --   all (\conn ->
    --     (connFromSide <$> conn) /= Just D20Up &&
    --     (connFromSide <$> conn) /= Just D20Left
    --     )
    --     sides
    --   )
    --   (M.toList connMap)))

    foo = M.toList connMap
      <&> _2 %~ (filter isJust >>> length)

    bar = filter (\(_,x) -> x==2) foo

    baz = map fst bar

    boo = map (imageMap M.!) baz

    image = genImage imageMap

  -- print m
  -- print $length m
  -- print $ head m
  -- print $ last m
  -- print $ length corners
  -- for_ connections print
  -- for_ numConnections print
  -- for_ corners print
  -- print ans
  -- print (M.toList connMap)
  -- print start
  print (M.size connMap)
  for_ bar print
  print baz

  -- first guy is north east corner
  -- second guy is south west corner
  -- third guy is south east corner
  -- forth guy is... south east corner
  -- ?????
  --
  -- Maybe I have to rotate one of the guys in the south east corner to get
  -- the north west corner.  I'll pick the last one, which is ID 3881.
  --
  -- Algorithm:
  -- First, rotate 3881 180 degrees, so that it becomes a north west corner
  -- piece.  This includes rotating the characters, but also rotating the
  -- D20Image connections (i.e. exchange north/south and west/east)
  --
  -- Next, go east until nothing, then south until nothing (similar to my
  -- genImage function)
  --
  -- For each piece after the first, and given the connection number of the
  -- previous piece:
  --
  -- For row:
  -- The first elem in the row is a special case.  For the first piece, do
  -- nothing, then the rest of the row. For other elems, rotate the chars s.t.
  -- the previous elem is north, then move onto the rest of the row (east).
  -- The "next" elem should always be the opposite of the "Nothing" side,
  -- since we're on an edge.
  --
  -- For the rest of the row (going east):
  -- Rotate the chars so that the connection faces west, then move to the
  -- next elem that is east.  Note that this isn't the 'original' east, it
  -- is the east after the rotations.  For example, after 3881 is 1019.  1019
  -- has 3881 as a north connection initially, so rotate that to the west,
  -- then the next east would be the opposite of that, south.  Basically,
  -- the "next" elem should always be the opposite of the previous elem, since
  -- the prevoius elem is west and the next elem is east.
  --
  -- CAVEAT: I don't know if flips might fuck this up.  I think they could,
  -- but it shouldn't change where the previous (west) and next (east) elems
  -- go.  However, I may have to inspect whether I need to "flip" the chars
  -- or not when I'm putting the elements in place.  This can be checked by
  -- inspecting the connection and determining whether this side matches
  -- the previous side, and if it doesn't, then the flip should match.
  for_ boo showConnections
  showConnections (d20Rotate180 (imageMap M.! 3881))
  showConnections (imageMap M.! 1019)
  showConnections (imageMap M.! 1747)
  showConnections (imageMap M.! 1291)
  showConnections (imageMap M.! 1709)
  showConnections (imageMap M.! 1499)
  -- for_ (tileChars $ imageMap M.! 3881) putStrLn
  -- putStrLn "-----------------------"
  -- for_ (tileChars $ d20Rotate180 $ imageMap M.! 3881) putStrLn

  print image


-- starting point defined as 3881 with 180 rotation
--
-- TODO: There's something I forgot to do, which is to check if I need to flip
-- the tile.  For genImageRow, I should only have to worry about a horizontal
-- flip because if I needed to flip vertically, then west/east would be
-- swapped, but that can't be, because I know that the west side matches the
-- previous tile's east side.
--
-- For rest, similar thoughts, but I know that I should only have to deal with
-- a vertical flip.
--
-- For both functions, the idea should be to compare the previous side against
-- the current side, and perform the flip if it doesn't match.  I need to get
-- the previous side by using imageMap M.! prev -> tileChars, and pick the
-- appropriate side.  I.e. for east side, map last; for south side, last.
-- It is unfortunate how inefficient this is, but idgaf.  Should be fine.
genImage :: Map Int D20Image -> [[[[Char]]]]
genImage imageMap = (tileChars start : genImageRow 3881 1019) : rest 3881 2039
  where
    start = d20Rotate180 (imageMap M.! 3881)

    genImageRow :: Int -> Int -> [[[Char]]]
    genImageRow prev curr = case next of
      Nothing -> [chars]
      Just next' -> chars : genImageRow curr next'
      where
        currImage = imageMap M.! curr

        rotationSide = if
          | connNorth currImage == Just prev -> D20Up
          | connSouth currImage == Just prev -> D20Down
          | connWest currImage == Just prev -> D20Left
          | connEast currImage == Just prev -> D20Right
          | otherwise -> error "Invalid connection"

        rotated = d20RotateToWest rotationSide currImage

        chars = tileChars rotated

        next = d20FromSide (d20Opposite rotationSide) currImage
          -- case d20Opposite rotationSide of
          -- D20Up -> connNorth currImage
          -- D20Down -> connSouth currImage
          -- D20Left -> connWest currImage
          -- D20Right -> connEast currImage

    rest :: Int -> Int -> [[[[Char]]]]
    rest prev curr = case d20FromSide (d20Opposite rotationSide) currImage of
      Nothing -> [ chars : genImageRow curr next ]
      Just nextSouth -> (chars : genImageRow curr next) : rest curr nextSouth
      where
        currImage = imageMap M.! curr

        rotationSide = if
          | connNorth currImage == Just prev -> D20Up
          | connSouth currImage == Just prev -> D20Down
          | connWest currImage == Just prev -> D20Left
          | connEast currImage == Just prev -> D20Right
          | otherwise -> error "Invalid connection"

        rotated = d20RotateToNorth rotationSide currImage

        chars = tileChars rotated

        -- The next elem will be the opposite of the Nothing side, with a
        -- special case for the last elem (bottom left corner), as there are
        -- two Nothing sides, and I want the opposite of the Nothing side that
        -- is NOT opposite to the previous side (i.e. I want east, not north)
        next =
          ( [D20Up, D20Down, D20Left, D20Right]
          \\ [rotationSide, d20Opposite rotationSide] )
          & map (`d20FromSide` currImage)
          & catMaybes
          & head

data D20Side = D20Up | D20Down | D20Left | D20Right -- | D20Flip D20Side
  deriving Eq

d20FromSide :: D20Side -> D20Image -> Maybe Int
d20FromSide = \case
  D20Up -> connNorth
  D20Down -> connSouth
  D20Left -> connWest
  D20Right -> connEast

d20Opposite :: D20Side -> D20Side
d20Opposite = \case
  D20Up -> D20Down
  D20Down -> D20Up
  D20Left -> D20Right
  D20Right -> D20Left
  -- D20Flip side -> D20Flip (d20Opposite side)

data D20Connection = D20Connection
  { connToID :: Int
  , connFromSide :: D20Side
  , connToSide :: D20Side
  }

-- Plan:
-- Map Int Image
-- Image = Image { tid, chars, northID, southID, westID, eastID }
-- Image' = Image' {tid, chars, northImage, southImage, westImage, eastImage }
-- Image' can probably be factored out
data D20Image = D20Tile
  { tileID :: Int
  , tileChars :: [[Char]]
  , connNorth :: Maybe Int
  , connSouth :: Maybe Int
  , connWest :: Maybe Int
  , connEast :: Maybe Int
  }
  deriving Show

showConnections :: D20Image -> IO ()
showConnections image = printf "%s %s %s %s\n"
    (show $ connNorth image)
    (show $ connSouth image)
    (show $ connWest image)
    (show $ connEast image)

-- data D20Image' = D20Tile'
--   { tileID :: Int
--   , tileChars :: [[Char]]
--   , connNorth :: Maybe D20Image'
--   , connSouth :: Maybe D20Image'
--   , connWest :: Maybe D20Image'
--   , connEast :: Maybe D20Image'
--   }

-- This function should create the D20Image' objects out of the information
-- that I have, which is tileID, connections of each tile, and (unrotated)
-- chars of each tile.  I need to rotate the chars to the correct orientation.
-- This function should start in the upper left and move East and South until
-- finished, just like my other genImage function.
--
-- Oh fuck I just realized my connection map is weird because when I rotate
-- a tile, I don't just rotate the chars, I have to rotate the orientation of
-- the connections as well, or something.  Gonna have to think about that
-- later.
--
-- Alright, here's the plan.  I'm making it so that this image map doesn't
-- actually rotate the tiles to the correct orientation.  Instead, it just
-- figures out which tile goes where with respect to each other, but doesn't
-- try to fix the orientation.  I will then take a pass over the tiles to
-- flip them to the right orientation.
genImageMap
  :: Int
  -> [[Char]]
  -> Map Int [Maybe D20Connection]
  -> Map Int D20Image
  -> Map Int D20Image
genImageMap tid chars connMap = M.insert tid image
  where
    image = D20Tile tid chars north south west east

    conns = catMaybes (connMap M.! tid)

    north = connToID <$> find (\conn -> connFromSide conn == D20Up) conns
    south = connToID <$> find (\conn -> connFromSide conn == D20Down) conns
    west = connToID <$> find (\conn -> connFromSide conn == D20Left) conns
    east = connToID <$> find (\conn -> connFromSide conn == D20Right) conns

-- genImage :: D20Image' -> Seq [[[Char]]]
-- genImage tile = case connSouth tile of
--   Nothing -> Seq.singleton (toList (genImageRow tile))
--   Just next -> toList (genImageRow tile) <| genImage next

-- genImageRow :: D20Image' -> Seq [[Char]]
-- genImageRow tile = case connEast tile of
--   Nothing -> Seq.singleton (tileChars tile)
--   Just next ->  tileChars tile <| genImageRow next

-- genImage :: D20Image' -> [[[[Char]]]]
-- genImage tile = case connSouth tile of
--   Nothing -> [genImageRow tile]
--   Just next -> genImageRow tile : genImage next

-- genImageRow :: D20Image' -> [[[Char]]]
-- genImageRow tile = case connEast tile of
--   Nothing -> [tileChars tile]
--   Just next ->  tileChars tile : genImageRow next

-- genImage :: Int -> Map Int D20Image -> [[[[Char]]]]
-- genImage tid tiles = case connSouth tile of
--   Nothing -> [genImageRow tid]
--   Just next -> genImageRow tid : genImage next tiles
--   where
--     tile = tiles M.! tid

--     genImageRow :: Int -> [[[Char]]]
--     genImageRow tid' = case connEast tile' of
--       Nothing -> [tileChars tile']
--       Just next ->  tileChars tile' : genImageRow next
--       where
--         tile' = tiles M.! tid'

d20Rotate90 :: D20Image -> D20Image
d20Rotate90 D20Tile { .. } = D20Tile
  { tileID = tileID
  , tileChars = chars'
  , connNorth = connEast
  , connSouth = connWest
  , connWest = connNorth
  , connEast = connSouth
  }
  where
    chars' = undefined

d20Rotate180 :: D20Image -> D20Image
d20Rotate180 D20Tile { .. } = D20Tile
  { tileID = tileID
  , tileChars = chars'
  , connNorth = connSouth
  , connSouth = connNorth
  , connWest = connEast
  , connEast = connWest
  }
  where
    chars' = reverse (map reverse tileChars)

d20RotateToWest :: D20Side -> D20Image -> D20Image
d20RotateToWest = \case
  D20Left -> id
  D20Right -> d20Rotate180
  D20Up -> d20Rotate90
  D20Down -> d20Rotate180 . d20Rotate90

d20RotateToNorth :: D20Side -> D20Image -> D20Image
d20RotateToNorth = \case
  D20Up -> id
  D20Down -> d20Rotate180
  D20Right -> d20Rotate90
  D20Left -> d20Rotate180 . d20Rotate90
