module Main where

import Data.Functor ((<&>))
import Data.List
-- import qualified Data.Text.IO as Text

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Helpers

main :: IO ()
main = d3p2

d1p1 :: IO ()
d1p1 = do
  nums :: [Int] <- readNums "data/d1.txt"
  print $ d1go nums

d1go :: [Int] -> Int
d1go [] = error "empty list"
d1go (x : xs) = snd $ foldl' f (x, 0) xs
  where
    f (a, acc) b = if b > a then (b, acc + 1) else (b, acc)

d1p2 :: IO ()
d1p2 = do
  nums :: [Int] <- readNums "data/d1.txt"
  let numsGrouped =
        zipWith3 (\a b c -> a + b + c) nums (tail nums) (tail (tail nums))
  print $ d1go numsGrouped

d2p1 :: IO ()
d2p1 = do
  cmds <- readLines "data/d2.txt" <&> map d2Parse
  let (hor, dep) = d2p1go cmds
  print (hor * dep)

data D2Command = Forward Int | Up Int | Down Int

d2Parse :: Text -> D2Command
d2Parse t =
  let split = Text.split (== ' ') t
      cmd = split !! 0
      x :: Int = tToI $ split !! 1
   in case cmd of
        "forward" -> Forward x
        "up" -> Up x
        "down" -> Down x
        _ -> error "invalid command"

d2p1go :: [D2Command] -> (Int, Int) -- horizontal, depth
d2p1go = foldl' f (0, 0)
  where
    f (hor, dep) = \case
      Forward x -> (hor + x, dep)
      Up x -> (hor, dep - x)
      Down x -> (hor, dep + x)

d2p2 :: IO ()
d2p2 = do
  cmds <- readLines "data/d2.txt" <&> map d2Parse
  let (hor, dep, _) = d2p2go cmds
  print (hor * dep)

d2p2go :: [D2Command] -> (Int, Int, Int) -- horizontal, depth, aim
d2p2go = foldl' f (0, 0, 0)
  where
    f (hor, dep, aim) = \case
      Forward x -> (hor + x, dep + aim * x, aim)
      Up x -> (hor, dep, aim - x)
      Down x -> (hor, dep, aim + x)

d3p1 :: IO ()
d3p1 = do
  diag <- readLines "data/d3.txt"
  let (gamma, epsilon) = d3p1go diag
      power = gamma * epsilon
  print power

d3p1go :: [Text] -> (Int, Int)
d3p1go diag = (gammaDec, epsilonDec)
  where
    gamma = d3p1helper diag 0
    epsilon = map (\c -> if c == '0' then '1' else '0') gamma
    gammaDec = fromMaybe 0 (readBin gamma)
    epsilonDec = fromMaybe 0 (readBin epsilon)

-- recursive: takes list of binaries and current index, finds binary gamma
d3p1helper :: [Text] -> Int -> [Char]
d3p1helper diag i =
  if (i >= Text.length (head diag))
    then []
    else
      let (zeroes, ones) = foldr f (0 :: Int, 0 :: Int) diag
          f b (z, o) = case Text.index b i of
            '0' -> (z + 1, o)
            '1' -> (z, o + 1)
            _ -> error "invalid binary string"
          c = if zeroes > ones then '0' else '1'
       in c : d3p1helper diag (i + 1)

d3p2 :: IO ()
d3p2 = do
  diag <- readLines "data/d3.txt"
  let (oxygen, co2) = d3p2go diag
      lifeSup = oxygen * co2
  print lifeSup

d3p2go :: [Text] -> (Int, Int)
d3p2go diag = (oxygen', co2')
  where
    oxygen = d3p2helper diag 0 (>) '1'
    co2 = d3p2helper diag 0 (<) '0'
    oxygen' = fromMaybe 0 (readBin (Text.unpack oxygen))
    co2' = fromMaybe 0 (readBin (Text.unpack co2))

-- diag, index, operation to decide which values to keep, default tie breaker
d3p2helper :: [Text] -> Int -> (Int -> Int -> Bool) -> Char -> Text
d3p2helper diag i op defaultC =
  if
      | i >= Text.length (head diag) -> head diag
      | length diag == 1 -> head diag
      | otherwise ->
        -- counts
        let (zeroes, ones) = foldr f (0 :: Int, 0 :: Int) diag
            f b (z, o) = case Text.index b i of
              '0' -> (z + 1, o)
              '1' -> (z, o + 1)
              _ -> error "invalid binary string"

            -- keep strings that match this
            toKeep =
              if zeroes == ones
                then defaultC
                else if zeroes `op` ones then '0' else '1'

            -- filtered diag
            diag' = filter (\t -> Text.index t i == toKeep) diag
         in d3p2helper diag' (i + 1) op defaultC
