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
-- import Text.Printf
import Control.Arrow ((>>>))
-- import Control.Algebra
-- import Control.Carrier.State.Strict
-- import Data.Sequence (Seq, Seq ((:<|), (:|>)))
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
main = d19p2



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
      oink xs ys = Set.map (\(x, y) -> x <> y) (Set.cartesianProduct xs ys)

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
