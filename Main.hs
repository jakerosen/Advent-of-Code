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

main :: IO ()
main = day2p2

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
