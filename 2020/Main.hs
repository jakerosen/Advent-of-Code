module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
-- import Data.Function ((&))
-- import Data.Functor ((<&>))
-- import Data.Either (fromRight)
import Data.Foldable (for_)
-- import Data.Foldable (traverse_)
-- import Data.Array (Array)
-- import qualified Data.Array as A
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Debug.Trace
import Data.List.Split
import Data.List
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Char
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.Either
-- import Control.Monad.State.Lazy (State)
-- import qualified Control.Monad.State.Lazy as S
import Control.Applicative
import Control.Lens hiding (children)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
-- import Text.Printf
import Control.Arrow ((>>>))
import Control.Algebra
import Control.Carrier.State.Strict
import Data.Sequence (Seq, Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
-- import Data.Word
import Data.Bits
import Numeric (readInt)
import Control.Monad.ST
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Applicative.Lift

main :: IO ()
main = d17p2

tToI :: Integral a => Text -> a
tToI x = T.signed T.decimal x
  & fromRight (trace (T.unpack x) undefined)
  & fst

tToIMaybe :: Integral a => Text -> Maybe a
tToIMaybe str = case T.decimal str of
  Left _ -> Nothing
  Right (x, _) -> Just x

d1p1 :: IO ()
d1p1 = do
  nums :: [Int] <- T.readFile "data/d1p1.txt"
    <&> T.lines
    <&> map tToI

  let
    ans = head [x * y | x <- nums, y <- nums, x + y == 2020]
  print ans

d1p2 :: IO ()
d1p2 = do
  nums :: [Int] <- T.readFile "data/d1p1.txt"
    <&> T.lines
    <&> map tToI

  let
    -- ans =
    --   head [x * y * z | x <- nums, y <- nums, z <- nums, x + y + z == 2020]

    -- more efficient (filter first)
    xs = [(x, y) | x <- nums, y <- nums, x + y <= 2020]
    ans2 = head [x * y * z | (x, y) <- xs, z <- nums, x + y + z == 2020]

  print $ ans2

d2p1Parse :: Text -> (Int, Int, Char, Text)
d2p1Parse entry = (tToI lo, tToI hi, c, pw)
  where
    [a, b, pw] = T.words entry
    [lo, hi] = T.split (=='-') a
    c = T.head b

d2p1Eval :: (Int, Int, Char, Text) -> Bool
d2p1Eval (lo, hi, c, pw) = count >= lo && count <= hi
  where
    count = T.length $ T.filter (==c) pw

d2p1 :: IO ()
d2p1 = do
  entries :: [(Int, Int, Char, Text)] <- T.readFile "data/d2p1.txt"
    <&> T.lines
    <&> map d2p1Parse
  print $ length $ filter d2p1Eval entries

myXor :: Bool -> Bool -> Bool
myXor x y = (x || y) && (not (x && y))

d2p2Parse :: Text -> (Int, Int, Char, Text)
d2p2Parse entry = (tToI lo, tToI hi, c, pw)
  where
    [a, b, pw] = T.words entry
    [lo, hi] = T.split (=='-') a
    c = T.head b

d2p2Eval :: (Int, Int, Char, Text) -> Bool
d2p2Eval (lo, hi, c, pw) = (a == c) `myXor` (b == c)
  where
    a = T.index pw (lo - 1)
    b = T.index pw (hi - 1)

d2p2 :: IO ()
d2p2 = do
  entries :: [(Int, Int, Char, Text)] <- T.readFile "data/d2p1.txt"
    <&> T.lines
    <&> map d2p2Parse
  print $ length $ filter d2p2Eval entries

-- acc, current iteration, slope (x,y), map -> count
d3p1Go :: Int -> Int -> (Int, Int) -> Vector (Vector Char) -> Int
d3p1Go acc i (slopeX, slopeY) m = if y >= V.length m
  then acc
  else
    let
      c = m ! y ! x
      newAcc = if c == '#' then acc + 1 else acc
    in d3p1Go newAcc (i + 1) (slopeX, slopeY) m
  where
    x = (slopeX * i) `mod` (length (m ! 0))
    y = slopeY * i

d3p1 :: IO ()
d3p1 = do
  m :: Vector (Vector Char) <- T.readFile "data/d3p1.txt"
    <&> T.lines
    <&> map (V.fromList . T.unpack)
    <&> V.fromList
  print $ d3p1Go 0 0 (3, 1) m

d3p2 :: IO ()
d3p2 = do
  m :: Vector (Vector Char) <- T.readFile "data/d3p1.txt"
    <&> T.lines
    <&> map (V.fromList . T.unpack)
    <&> V.fromList
  let
    slopes = map (flip (d3p1Go 0 0) m)
      [ (1,1)
      , (3,1)
      , (5,1)
      , (7,1)
      , (1,2) ]
  for_ slopes print
  print $ product slopes

d4p1EvalPass :: [Text] -> Bool
d4p1EvalPass fields = allMandPresent && noExtras
  where
    allMandPresent = null $ mandFields \\ presFields
    noExtras = null $ presFields \\ allFields

    presFields :: [Text]
    presFields = map (head . T.split (==':')) fields

    allFields :: [Text]
    allFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

    mandFields :: [Text]
    mandFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

d4p1 :: IO ()
d4p1 = do
  m <- T.readFile "data/d4p1.txt"
    <&> T.lines
    <&> splitOn [""]
  let passports = map (concatMap T.words) m
  print $ length $ filter d4p1EvalPass passports

d4p2Hcl :: Text -> Bool
d4p2Hcl hcl = T.length hcl == 7 && T.head hcl == '#' &&
  T.all (\c -> isDigit c || c `elem` ['a'..'f']) (T.tail hcl)

d4p2EvalPass :: Map Text Text -> Bool
d4p2EvalPass passport = byr && iyr && eyr && hgt && hcl && ecl && pid
  where
    byr = case M.lookup "byr" passport >>= tToIMaybe of
      Nothing -> False
      Just (x :: Int) -> x >= 1920 && x <= 2002

    iyr = case M.lookup "iyr" passport >>= tToIMaybe of
      Nothing -> False
      Just (x :: Int) -> x >= 2010 && x <= 2020

    eyr = case M.lookup "eyr" passport >>= tToIMaybe of
      Nothing -> False
      Just (x :: Int) -> x >= 2020 && x <= 2030

    hgt = case M.lookup "hgt" passport of
      Nothing -> False
      Just h -> case T.decimal h of
        Left _ -> False
        Right ((x :: Int), unit) -> case unit of
          "cm" -> x >= 150 && x <= 193
          "in" -> x >= 59 && x <= 76
          _ -> False

    hcl = case M.lookup "hcl" passport of
      Nothing -> False
      Just hc -> d4p2Hcl hc

    ecl = case M.lookup "ecl" passport of
      Nothing -> False
      Just ec -> ec `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

    pid = case M.lookup "pid" passport of
      Nothing -> False
      Just p -> T.length p == 9 && T.all isDigit p

keySplit :: Text -> (Text, Text)
keySplit t = (head keyVal, keyVal !! 1)
  where
    keyVal = T.split (==':') t

d4p2 :: IO ()
d4p2 = do
  m <- T.readFile "data/d4p1.txt"
    <&> T.lines
    <&> splitOn [""]
  let
    passports :: [Map Text Text]
    passports = map (M.fromList . map keySplit . concatMap T.words) m

  print $ length $ filter d4p2EvalPass passports

d5p1NarrowRange :: Int -> Int -> Int
d5p1NarrowRange lo hi = (hi - lo) `div` 2 + lo

d5p1GetRow :: [Char] -> Int
d5p1GetRow seat = go seat (0,127)
  where
    go :: [Char] -> (Int, Int) -> Int
    go [] (lo, _) = lo
    go (x:xs) (lo, hi) = if lo == hi then lo else
      case x of
        'F' -> go xs (lo, d5p1NarrowRange lo hi)
        'B' -> go xs (d5p1NarrowRange lo hi + 1, hi)
        _ -> error "Invalid seat code"

d5p1GetCol :: [Char] -> Int
d5p1GetCol seat = go seat (0,7)
  where
    go :: [Char] -> (Int, Int) -> Int
    go [] (lo, _) = lo
    go (x:xs) (lo, hi) = if lo == hi then lo else
      case x of
        'L' -> go xs (lo, d5p1NarrowRange lo hi)
        'R' -> go xs (d5p1NarrowRange lo hi + 1, hi)
        _ -> error "Invalid seat code"

d5p1Decode :: [Char] -> Int
d5p1Decode seat = row * 8 + col
  where
    (xs, ys) = splitAt 7 seat
    row = d5p1GetRow xs
    col = d5p1GetCol ys

d5p1 :: IO ()
d5p1 = do
  m :: [Int] <- T.readFile "data/d5p1.txt"
    <&> T.lines
    <&> map T.unpack
    <&> map d5p1Decode

  print $ maximum m

d5p2 :: IO ()
d5p2 = do
  m :: [Int] <- T.readFile "data/d5p1.txt"
    <&> T.lines
    <&> map T.unpack
    <&> map d5p1Decode

  let
    foo = [0..955] \\ m
    boo = filter (\x -> (x-1) `elem` m && (x+1) `elem` m) foo

  for_ boo print

d6p1 :: IO ()
d6p1 = do
  m <- T.readFile "data/d6p1.txt"
    <&> T.lines
    <&> splitOn [""]

  let
    groups = map T.concat m
    answered = map (intersect ['a'..'z'] . T.unpack) groups
    ans = sum $ map length answered

  print ans
  -- for_ (map length answered) print
  -- for_ m \x -> do
  --   for_ x print
  --   putStrLn "________"

d6p2 :: IO ()
d6p2 = do
  m <- T.readFile "data/d6p1.txt"
    <&> T.lines
    <&> splitOn [""]

  let
    answered = map (foldl' intersect ['a'..'z'] . map T.unpack) m
    ans = sum $ map length answered

  print ans

d7p1ParseLine :: Text -> (Text, [Text])
d7p1ParseLine line = (parent, children)
  where
    foo = T.splitOn " bags contain " line

    parent = head foo
    boo = foo !! 1
    cs = T.splitOn ", " boo

    children = if head cs == "no other bags."
      then []
      else map parseChild cs

    parseChild :: Text -> Text
    parseChild child = child
      & T.words -- break into words
      & init . tail -- throw away first and last elements
      & T.intercalate " " -- combine back into words, bag name

d7p1Go :: Set Text -> [Text] -> Map Text [Text] -> Set Text
d7p1Go seen [] _ = seen
d7p1Go seen (x:stack) parents = if S.member x seen
  then d7p1Go seen stack parents
  else d7p1Go
    (S.insert x seen)
    (M.findWithDefault [] x parents ++ stack)
    parents

d7p1 :: IO ()
d7p1 = do
  ls :: [Text] <- T.readFile "data/d7p1.txt"
    <&> T.lines

  let
    prepped :: [(Text, [Text])]
    prepped = map d7p1ParseLine ls

    foo :: [(Text, Text)]
    foo = concatMap sequence prepped

    boo :: [(Text, Text)]
    boo = map swap foo

    loo :: [(Text, [Text])]
    loo = map (\(a, b) -> (a, [b])) boo

    -- children :: Map Text [Text]
    -- children = M.fromListWith (++) prepped

    parents :: Map Text [Text]
    parents = M.fromListWith (++) loo

  -- traverse_ print (children !? "shiny gold")
  -- traverse_ print (parents !? "shiny gold")
  -- traverse_ print $ parents !? "dark green"
  print $ S.size $ d7p1Go S.empty (parents M.! "shiny gold") parents

-- This actually gives an answer that's 1 higher than the actual answer, and
-- I sort of see why, but I don't know how to fix it.  Basically, the
-- accumulator starts at because the default value of foldl' is 1, but it
-- should start at 0.  However, if I set the default value of foldl' to 0, the
-- algorithm doesn't work because nothing ever increases the value from 0.
-- In short, this algorithm incorrectly counts the initial bag itself, but it
-- shouldn't, and I can't fix it.
d7p2Go :: Text -> Map Text [(Int, Text)] -> Int
d7p2Go x children = foldl' (\acc (n, t) -> acc + n * d7p2Go t children) 1 cs
  where
    cs :: [(Int, Text)]
    cs = M.findWithDefault [] x children

d7p2ParseLine :: Text -> (Text, [(Int, Text)])
d7p2ParseLine line = (parent, children)
  where
    foo = T.splitOn " bags contain " line

    parent = head foo
    boo = foo !! 1
    cs = T.splitOn ", " boo

    children = if head cs == "no other bags."
      then []
      else map parseChild cs

    parseChild :: Text -> (Int, Text)
    parseChild child = child
      & T.words -- break into words
      & init . tail -- throw away first and last elements
      & T.intercalate " " -- combine back into words, bag name
      & \t -> (tToI (head (T.words child)), t) -- combine with number of bags

d7p2 :: IO ()
d7p2 = do
  ls :: [Text] <- T.readFile "data/d7p1.txt"
    <&> T.lines

  let
    prepped :: [(Text, [(Int, Text)])]
    prepped = map d7p2ParseLine ls

    children :: Map Text [(Int, Text)]
    children = M.fromListWith (++) prepped

  print $ d7p2Go "shiny gold" children - 1

data D8Instr = Nop Int | Acc Int | Jmp Int
  deriving Show

parseInstr :: Text -> D8Instr
parseInstr t = case ins of
  "nop" -> Nop param
  "acc" -> Acc param
  "jmp" -> Jmp param
  _ -> error "Invalid instruction"
  where
    foo = T.words t
    ins = head foo
    param = tToI $ foo !! 1

-- seen instr, acc, current instr, list of instr -> acc
d8p1Go :: Set Int -> Int -> Int -> Vector D8Instr -> Int
d8p1Go seen acc cur v = if S.member cur seen
  then acc
  else case ins of
    Nop _ -> d8p1Go (S.insert cur seen) acc (cur+1) v
    Acc p -> d8p1Go (S.insert cur seen) (acc+p) (cur+1) v
    Jmp p -> d8p1Go (S.insert cur seen) acc (cur+p) v
  where
    ins = v ! cur

d8p1 :: IO ()
d8p1 = do
  is :: Vector D8Instr <- T.readFile "data/d8p1.txt"
    <&> T.lines
    <&> map parseInstr
    <&> V.fromList

  print $ d8p1Go S.empty 0 0 is

-- seen instr, acc, current instr, list of instr -> acc OR "Infinite Loop"
d8p2Go :: Set Int -> Int -> Int -> Vector D8Instr -> Either String Int
d8p2Go seen acc cur v = case insM of
  Nothing -> Right acc -- We've reached the end, terminate successfully
  Just ins -> if S.member cur seen
    then Left "Infinite Loop"
    else case ins of
      Nop _ -> d8p2Go (S.insert cur seen) acc (cur+1) v
      Acc p -> d8p2Go (S.insert cur seen) (acc+p) (cur+1) v
      Jmp p -> d8p2Go (S.insert cur seen) acc (cur+p) v
  where
    insM = v V.!? cur

d8p2 :: IO ()
d8p2 = do
  is :: Vector D8Instr <- T.readFile "data/d8p1.txt"
    <&> T.lines
    <&> map parseInstr
    <&> V.fromList

  let
    candidates :: [Vector D8Instr]
    candidates = catMaybes $ map switchMaybe [0 .. V.length is - 1]

    switchMaybe :: Int -> Maybe (Vector D8Instr)
    switchMaybe i = case is ! i of
      Nop p -> Just $ is // [(i, Jmp p)]
      Acc _ -> Nothing -- for efficiency -- no reason to compute extra
      Jmp p -> Just $ is // [(i, Nop p)]

    fixed :: [Int]
    fixed = rights $ map (d8p2Go S.empty 0 0) candidates

  for_ fixed print

-- This is really ugly, but basically I'm doing an ugly implementation of
-- ifilter, but I need access to the original vector.  Also, I'm just throwing
-- out the preamble since I don't want it in the result.
d9p1Eval :: Vector Int -> Int -> Int -> Bool
d9p1Eval v i n = if (i < 25) then False
  else n `notElem` [x + y | x <- sliced, y <- sliced, x /= y]
  where
    sliced :: [Int]
    sliced = V.toList $ V.slice (i - 25) 25 v

d9p1 :: IO ()
d9p1 = do
  v :: Vector Int <- T.readFile "data/d9p1.txt"
    <&> T.lines
    <&> map tToI
    <&> V.fromList

  let
    criticals = V.ifilter (d9p1Eval v) v

  for_ criticals print

-- critical num, vector -> sequence
d9p2FindWeakness :: Int -> Vector Int -> [Int]
d9p2FindWeakness x v = go 0
  where
    -- acc, seq, current index -> seq if found or Nothing if sequence failed
    evalSeq :: Int -> [Int] -> Int -> Maybe [Int]
    evalSeq acc sequ j = if
      | acc == x -> Just sequ -- We found the sequence
      | acc > x -> Nothing -- failed sequence, move on
      | acc < x -> case v V.!? j of
        Nothing -> Nothing
        Just val ->
          evalSeq (acc + val) (val:sequ) (j+1) -- keep evaluating
      | otherwise -> error "wut how was I getting a non-exhaustive warning"

    go :: Int -> [Int]
    go i
      | i > V.length v = error "wut no sequence found"
      | otherwise = case evalSeq 0 [] i of
        Nothing -> go (i+1)
        Just sequ -> sequ

d9p2 :: IO ()
d9p2 = do
  v :: Vector Int <- T.readFile "data/d9p1.txt"
    <&> T.lines
    <&> map tToI
    <&> V.fromList

  let
    critical = V.head $ V.ifilter (d9p1Eval v) v
    foo = V.filter (<=critical) v
    weakness = d9p2FindWeakness critical foo

  print critical
  print (V.length v)
  print (V.length foo)
  print $ weakness
  print $ maximum weakness + minimum weakness

d10p1 :: IO ()
d10p1 = do
  m :: [Int] <- T.readFile "data/d10p1.txt"
    <&> T.lines
    <&> map tToI
    <&> (\l -> 0: maximum l + 3 :l) -- add the outlet and device
    <&> sort

  let
    distribution :: Map Int Int
    distribution = M.empty

    foo = zipWith (\x y -> abs (x - y)) m (tail m)
    boo = foldl'
      (\m' x -> M.insert x (M.findWithDefault 0 x m' + 1) m') distribution foo

    -- goo = M.insert 3 (M.findWithDefault 0 3 boo + 1) boo

  print $ liftA2 (*) (boo !? 1) (boo !? 3)

d10p2CountPerms :: [Int] -> Int
d10p2CountPerms diffs = product $ go (group diffs)
  where
    -- grouped = trace (show sorted) (group diffs)
    -- sorted = sortBy (\x y -> compare (length x) (length y)) (group diffs)

    go :: [[Int]] -> [Int]
    go [] = []
    go (x:xs) = if head x == 3 then go xs else
      -- if there are 4 1s in a row, then the permutation of "all missing"
      -- does NOT work because you can't reach the next adapter. There would
      -- actually be a different pattern for greater numbers of 1s in a row,
      -- but none such exist in the input (this function would not work in that
      -- case).
      if length x >= 4
        then 2^(length x - 1) - 1 : go xs
        else 2^(length x - 1) : go xs

-- 0 1 2 3 6
-- MUST HAVE 0 3 6
-- 0 1 3 6
-- 0 2 3 6
-- 0 3 6
--
-- OPTIONAL 1 2
-- PERMS [1, 2] = [] [1] [2] [1 2]
--
-- algorithm:
-- identify which are MUST HAVE and which are OPTIONAL, then do permutations
-- of OPTIONAL
--
-- MUST HAVE is identified by the end points and both sides when there is a
-- difference of 3. So in the list 0 1 2 3 6 7, 0 and 7 are considered MUST
-- HAVE because they are end points, and 3 and 6 are MUST HAVE because there
-- is a difference of 3 between them. 1 and 2 are OPTIONAL, so you do the
-- permutations between them and you get 4 total arrangements, which are:
-- 0 1 2 3 6 7
-- 0 1 3 6 7
-- 0 2 3 6 7
-- 0 3 6 7
--
-- Remember not to accidentally double count end points.  For example, the
-- device is, by definition, always +3 over the highest adapter, so if you
-- count it as a difference of 3 and also as an end point, you count it twice.
-- In this case, you would want to not count it as an endpoint because the +3
-- is already counted.  Therefore, the only thing that needs to be considered
-- is the outlet, which is always 0, and the first chargers.  Because I already
-- know the input, the first items are in fact 0 1 2 3, so 0 should be counted
-- as an endpoint.
--
-- 0 1 2 3 6
-- diffs:
-- 1 1 1 3
-- take away # of 3s * 2 and 1 more for begin endpoint
-- 5 - 1 * 2 - 1 = 2
-- 2^2 = 4
--
-- This seems to work
--
-- 0 1 4 5 6 7 10 11 12 15 16 19 22
--
-- I see, the problem is that 2 3s in a row double counts one of them.
--
-- Problem:
-- 0 1 2 3 6 7 8 9 10 13
-- diffs:
-- 1 1 1 3 1 1 1 1 3
-- grouped:
-- [1 1 1] [3] [1 1 1 1] [3]
--
-- The problem here is that in the set of 4 differences of 1 in a row, where
-- my flex count function thinks there should be 3 flex slots, that doesn't
-- actually work because you can't be missing all of them or you can't reach
-- the upper level adapter.  I.e., while 7 8 and 9 are all correctly flex slots,
-- it is NOT the case that you can be missing all of them.
-- 6 7 8 9 10
-- 6 8 9 10
-- 6 7 9 10
-- 6 7 8 10
-- and so forth work as perms, but
-- 6 10 does not
--
-- This problem persists beyond just 4 1s n a row, but it seems that there are
-- no such groups in the actual input, so a lazy fix seems like it should work.
-- The lazy fix is just "don't count missing all of them" (in other words, add
-- 1 if there are 4 1s in a row).
--
-- Just for fun, how would 5 1s in a row behave?
-- 6 7 8 9 10 11 14
-- The ones that don't count would be:
-- 6 11
-- 6 7 11
-- 6 10 11
--
-- All other basic perms would work.  So 4 1s in a row has 1 that doesn't work,
-- 5 1s in a row has 3 that don't work.  Ok rather than think about number of
-- 1s in a row in the diff groups, how about thinking in terms of the number
-- of flex slots to derive permutations from.  So with 2 flex slots, you get
-- 4 total permutations and all work.  3 flex slots, you get 8 total perms, but
-- 1 doesn't work.  4 flex slots you get 16 total perms, but 3 don't work, and
-- here, we can see that each "side" has a perm of 1 item that doesn't work in
-- addition to the all missing case that doesn't work, so it's sort of like
-- 1 + 2 * perm 1 = 3 that don't work.  3 flex slots are 1 + 2 * perm 0 = 1 that
-- won't work.  By that pattern, seemingly the next would be 5 flex slots for
-- 32 perms total but 1 + 2 * perm 2 = 5 don't work.  I think that works.
--
-- So the pattern for when there are 3 or more flex slots is
-- perms n - (1 + 2 * perms (n - 3))
d10p2 :: IO ()
d10p2 = do
  m :: [Int] <- T.readFile "data/d10p1.txt"
    <&> T.lines
    <&> map tToI
    <&> (\l -> 0: maximum l + 3 :l) -- add the outlet and device
    <&> sort

  let
    -- m = [(0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)]

    distribution :: Map Int Int
    distribution = M.empty

    diffs = zipWith (\x y -> abs (x - y)) m (tail m)
    dist' = foldl'
      (\m' x -> M.insert x (M.findWithDefault 0 x m' + 1) m') distribution diffs

    -- n = length m - (M.findWithDefault 0 3 dist' * 2) - 1
    -- n = d10p2CountPerms diffs
    -- n = 49
    perms = d10p2CountPerms diffs

    -- sorted = sortBy (\x y -> compare (length x) (length y)) (group diffs)

  -- too low: 549755813888 (n = 39)
  -- too high: 562949953421312 (n = 49)
  print m
  print diffs
  print $ group diffs
  print $ length m
  print (dist' !? 1)
  print (dist' !? 3)
  -- putStr "n: "
  -- print $ n
  -- print $ 2^n
  print perms
  print $ (logBase 2 (fromIntegral perms) :: Double)

d11p1ChangeSeat :: Vector (Vector Char) -> Int -> Int -> Char -> Char
d11p1ChangeSeat seats i j seat = case seat of
  'L' -> if all (/='#') adj then '#' else 'L'
  '#' -> if length (filter (=='#') adj) >= 4 then 'L' else '#'
  '.' -> '.'
  _ -> error $ "Invalid seat: " ++ [seat]
  where
    adj :: [Char]
    adj = catMaybes $
      [ getSeatMaybe x y
      | x <- [i-1..i+1]
      , y <- [j-1..j+1]
      , not (x == i && y == j)
      ]

    getSeatMaybe :: Int -> Int -> Maybe Char
    getSeatMaybe x y = seats V.!? x >>= (V.!? y)

d11p1 :: IO ()
d11p1 = do
  m :: Vector (Vector Char) <- T.readFile "data/d11p1.txt"
    <&> T.lines
    <&> map (V.fromList . T.unpack)
    <&> V.fromList

  let
    untilDup :: Vector (Vector Char) -> Vector (Vector Char)
    untilDup seats = if seats == seats'
      then seats'
      else untilDup seats'
      where
        seats' = V.imap (\i v -> V.imap (d11p1ChangeSeat seats i) v) seats

    predictedSeats = untilDup m
    occupied = V.sum $ V.map (V.length . V.filter (=='#')) predictedSeats

  -- for_ (untilDup m) print
  print occupied

d11p2ChangeSeat :: Vector (Vector Char) -> Int -> Int -> Char -> Char
d11p2ChangeSeat seats i j seat = case seat of
  'L' -> if all (/='#') adj then '#' else 'L'
  '#' -> if length (filter (=='#') adj) >= 5 then 'L' else '#'
  '.' -> '.'
  _ -> error $ "Invalid seat: " ++ [seat]
  where
    adj :: [Char]
    adj = catMaybes $
      [ findNearestSeatMaybe x y
      | x <- [(-1)..1]
      , y <- [(-1)..1]
      , not (x == 0 && y == 0)
      ]

    getSeatMaybe :: Int -> Int -> Maybe Char
    getSeatMaybe x y = seats V.!? x >>= (V.!? y)

    findNearestSeatMaybe :: Int -> Int -> Maybe Char
    findNearestSeatMaybe x y = go (i+x) (j+y)
      where
        go :: Int -> Int -> Maybe Char
        go i' j' = do
          s <- getSeatMaybe i' j'
          if s == '.'
            then go (i' + x) (j' + y)
            else Just s
          -- case getSeatMaybe i' j' of
          --   Nothing -> Nothing
          --   Just s -> if s == '.' then go (i' + x) (j' + y) else Just s

d11p2 :: IO ()
d11p2 = do
  m :: Vector (Vector Char) <- T.readFile "data/d11p1.txt"
    <&> T.lines
    <&> map (V.fromList . T.unpack)
    <&> V.fromList

  let
    untilDup :: Vector (Vector Char) -> Vector (Vector Char)
    untilDup seats = if seats == seats'
      then seats'
      else untilDup seats'
      where
        seats' = V.imap (\i v -> V.imap (d11p2ChangeSeat seats i) v) seats

    predictedSeats = untilDup m
    occupied = V.sum $ V.map (V.length . V.filter (=='#')) predictedSeats

  -- for_ (untilDup m) print
  print occupied

data D12Instr
  = North Int
  | South Int
  | East Int
  | West Int
  | TurnL Int
  | TurnR Int
  | Forward Int
  deriving (Show, Eq)

d12p1ParseInstr :: (Text, Text) -> D12Instr
d12p1ParseInstr (instr, p) = case instr of
  "N" -> North (tToI p)
  "S" -> South (tToI p)
  "E" -> East (tToI p)
  "W" -> West (tToI p)
  "L" -> TurnL (tToI p)
  "R" -> TurnR (tToI p)
  "F" -> Forward (tToI p)
  _ -> error "Bad instruction"

data D12p1State = D12p1State
  { d12p1Y :: Int
  , d12p1X :: Int
  , d12p1Facing :: Int
  } deriving stock (Show, Generic)

d12p1NextState :: D12p1State -> D12Instr -> D12p1State
d12p1NextState cur instr = cur & case instr of
  North p -> #d12p1Y %~ (+p)
  South p -> #d12p1Y %~ (subtract p)
  East p -> #d12p1X %~ (+p)
  West p -> #d12p1X %~ (subtract p)
  TurnL p -> #d12p1Facing %~ (+p)
  TurnR p -> #d12p1Facing %~ (subtract p)
  Forward p -> case (d12p1Facing cur) `mod` 360 of
    0 -> #d12p1X %~ (+p) -- east
    90 -> #d12p1Y %~ (+p) -- north
    180 -> #d12p1X %~ (subtract p) -- west
    270 -> #d12p1Y %~ (subtract p) -- south
    e -> error ("Invalid facing: " ++ show e)

d12p1 :: IO ()
d12p1 = do
  m :: [D12Instr] <- T.readFile "data/d12p1.txt"
    <&> T.lines
    <&> map (T.splitAt 1)
    <&> map d12p1ParseInstr

  let
    finalState = foldl' d12p1NextState (D12p1State 0 0 0) m
    manhattan = abs (finalState ^. #d12p1X) + abs (finalState ^. #d12p1Y)


  -- traverse_ print
  --   (filter (\case TurnL _ -> True; TurnR _ -> True; _ -> False) m)
  print manhattan

data D12p2State = D12p2State
  { d12p2WayPointY :: Int
  , d12p2WayPointX :: Int
  , d12p2ShipY :: Int
  , d12p2ShipX :: Int
  } deriving stock (Show, Generic)

d12p2NextState :: D12p2State -> D12Instr -> D12p2State
d12p2NextState cur instr = case instr of
  North p -> cur & #d12p2WayPointY %~ (+p)
  South p -> cur & #d12p2WayPointY %~ (subtract p)
  East p -> cur & #d12p2WayPointX %~ (+p)
  West p -> cur & #d12p2WayPointX %~ (subtract p)
  TurnL p -> turn (p `mod` 360)
  TurnR p -> turn ((negate p) `mod` 360)
  Forward p -> cur
    & #d12p2ShipY %~ (+ p * (cur ^. #d12p2WayPointY))
    & #d12p2ShipX %~ (+ p * (cur ^. #d12p2WayPointX))
  where
    -- 5N 4E is equivalent to (4, 5)
    -- rotate left by 90 degrees means...
    -- N becomes W and E becomes N
    -- 5W 4N equivalent to (-5, 4)
    -- rotate again...
    -- 5S 4W = (-4, -5)
    -- For rotate left by 90, swap and negate X seems to work
    --
    -- rotate 180 is just negate both
    --
    -- rotate 270 is swap and negate Y?
    -- 5N 4E = (4, 5)
    -- 5E 4S = (5, -4)
    -- Looks like it
    turn :: Int -> D12p2State
    turn = \case
      0 -> cur -- no rotation
      90 -> flipXY & #d12p2WayPointX %~ negate
      180 -> cur
        & #d12p2WayPointX %~ negate
        & #d12p2WayPointY %~ negate
      270 -> flipXY & #d12p2WayPointY %~ negate
      _ -> error "Invalid rotation"

    flipXY :: D12p2State
    flipXY = cur
      & #d12p2WayPointX .~ (cur ^. #d12p2WayPointY)
      & #d12p2WayPointY .~ (cur ^. #d12p2WayPointX)

d12p2 :: IO ()
d12p2 = do
  m :: [D12Instr] <- T.readFile "data/d12p1.txt"
    <&> T.lines
    <&> map (T.splitAt 1)
    <&> map d12p1ParseInstr

  let
    finalState = foldl' d12p2NextState (D12p2State 1 10 0 0) m
    manhattan =
      abs (finalState ^. #d12p2ShipX) + abs (finalState ^. #d12p2ShipY)

  print manhattan

d13p1 :: IO ()
d13p1 = do
  m :: (Int, [Int]) <- T.readFile "data/d13p1.txt"
    <&> T.lines
    <&> splitAt 1
    <&> both %~ head
    <&> _1 %~ tToI
    <&> _2 %~ catMaybes . map tToIMaybe . T.split (==',')

  let
    (earliest, busses) = m
    ttWait = map (\x -> x - rem earliest x) busses
    foo = zip busses ttWait
    bar = minimumBy (\(_, a) (_, b) -> compare a b) foo
    ans = fst bar * snd bar

  print $ ans

d13p2Go :: [Maybe Integer] -> Integer
d13p2Go s = fromJust $ find testTS [0, first ..]
  where
    first :: Integer
    first = snd $ head withOffsets

    -- foo :: Integer
    -- foo = 100000000000000 - rem 100000000000000 first

    withOffsets :: [(Integer, Integer)]
    withOffsets = catMaybes $ zipWith f [0..] s
      where
        f :: Integer -> Maybe Integer -> Maybe (Integer, Integer)
        f offset = (>>= \x -> Just (offset, x))

    -- This works, but I can't test each number individually because the number
    -- of numbers I have to test is way too large
    testTS :: Integer -> Bool
    testTS ts = all (\(offset, x) -> rem (ts + offset) x == 0) withOffsets

d13p2Test :: [Maybe Integer] -> [Integer]
d13p2Test s = filter testTS [0, first ..]
  where
    first :: Integer
    first = snd $ head withOffsets

    -- foo :: Integer
    -- foo = 100000000000000 - rem 100000000000000 first

    withOffsets :: [(Integer, Integer)]
    withOffsets = catMaybes $ zipWith f [0..] s
      where
        f :: Integer -> Maybe Integer -> Maybe (Integer, Integer)
        f offset = (>>= \x -> Just (offset, x))

    -- This works, but I can't test each number individually because the number
    -- of numbers I have to test is way too large
    testTS :: Integer -> Bool
    testTS ts = all (\(offset, x) -> rem (ts + offset) x == 0) withOffsets

-- d13p2Combine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
-- d13p2Combine a b = (newOffset, newNumber)
--   where
--     testTS :: Integer -> Bool
--     testTS ts = all (\(offset, x) -> rem (ts + offset) x == 0) [a, b]

--     newOffset :: Integer
--     newOffset = fromJust $ find testTS [0..]

--     newNumber :: Integer
--     newNumber = snd a * snd b

-- We're going to treat the first number and second number differently, so
-- order definitely matters.  Use foldl1'.
d13p2Combine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
d13p2Combine (xoff,x) (yoff,y) =
  -- trace
  --   (printf "combine: (%d,%d) and (%d,%d): (%d,%d)" xoff x yoff y
  --     newOffset newNumber)
    (newOffset, newNumber)
  where
    testTS :: Integer -> Bool
    testTS ts = all (\(offset, xx) -> rem (ts + offset) xx == 0) [a, b]

    newOffset :: Integer
    newOffset = fromJust (find testTS [x, x * 2 ..]) + xoff

    newNumber :: Integer
    newNumber = x * y

    a = (0, x)
    b = (xoff+yoff, y)

d13p2Go2 :: [Maybe Integer] -> Integer
d13p2Go2 s = fst $ foldl1' d13p2Combine withOffsets
  where
    withOffsets :: [(Integer, Integer)]
    withOffsets = catMaybes $ zipWith f [0..] s
      where
        f :: Integer -> Maybe Integer -> Maybe (Integer, Integer)
        f offset = (>>= \x -> Just (offset, x))

-- BREAKTHROUGH
--
-- When testing the list [67, 7, ...], I wanted to see if I could "put things
-- together 1 by 1", so I focused on 67 and 7.  This would be 67 with offset
-- 0 and 7 with offset 1.  First, I found multiples of 67 and multiples of 7,
-- and I was looking for a multiple of 67 that was exactly 1 smaller than a
-- multiple of 7, which would be the first meeting point.  This would be "the
-- answer" if these were the only 2 numbers in the list.  This number was 335,
-- and it is indeed the answer using my function to look for the answer.  Good.
--
-- The question after that is, what is the next time those numbers meet?  If
-- we're going to be putting numbers together 1 by 1, then whatever other
-- numbers are added on are still going to have to conform to the meeting
-- points between 67 and 7.  This is where my tester function, d13p2Test comes
-- in.  It finds the infinite list of all meeting points, and I can examine
-- however many I want to.  The next meeting point between 67 and 7 is 804.  At
-- first blush, this number seems quite random!  It's certainly not anywhere
-- close to double 335, or even something like double 335 + 1 or whatever,
-- accounting for timestamp 0.  Actually, it's 469 greater than 335.  469 also
-- happens to be 67 * 7!  And every number in the list after that is +469 over
-- the previous number.  That means that the meeting points between any two
-- numbers are easy to predict.  What happens if we now treat 469 as our new
-- "number", and have an offset of 335?  Could we then "combine" the next
-- number?  Looking promising!
--
-- So basically we're folding the list where our folding operation is:
-- foldOp :: (offset, number) -> (offset, number) -> (offset, number)
-- \(ox, x) (oy, y) ->
-- new offset = <use function that finds first meeting point>
-- new number = x * y
--
-- I feel like I'm on the right track, but the answer isn't quite right yet.
--
-- NO WAY.  This is too slow too.  Ugh.  I got it working, but it can't
-- compute the answer.  I did not expect that.  Although now that I look at it,
-- I can see why.  My combine function computes the new offset by looking for
-- a timestamp through all possible numbers.  Actually, it's doing basically
-- the same thing as my previous function.  I need to figure out how to do that
-- in a more clever way if I am to actually compute the answer.
--
-- WOOOOOOOOO!  I got it.  All I had to do was change [0..] (step through all
-- positive integers 1 by 1) to [x, x * 2 ..] (i.e, I only need to look at
-- multiples of the first number).  Before, when I was thinking of something
-- like multiples of the first number in the list (i.e. like 29), that number
-- was far too small to hope to cut down the trillions of numbers that I had
-- to look through.  100000000000000 divided by 29 is still over a trillion,
-- so that computation would never finish.  However, in my new way of doing it,
-- the accumulator that builds as I combine numbers becomes the product of all
-- numbers that came before it, and I know the answer had to be some multiple
-- of all of those numbers, and so I could look through only those.  That
-- number was likely in the trillions itself, so unlike 29, looking through
-- only multiples of those numbers cut down the computation enough to easily
-- find the answer.  Whew!
-- Answer: 780601154795940
d13p2 :: IO ()
d13p2 = do
  m :: [Maybe Integer] <- T.readFile "data/d13p1.txt"
    <&> T.lines
    <&> head . snd . splitAt 1
    <&> map tToIMaybe . T.split (==',')

  let
    -- withOffsets :: [Maybe Integer] -> [(Integer, Integer)]
    -- withOffsets l = catMaybes $ zipWith f [0..] l
    --   where
    --     f :: Integer -> Maybe Integer -> Maybe (Integer, Integer)
    --     f offset = (>>= \x -> Just (offset, x))

    -- ans = d13p2Go m

    -- (0,2) | (1,3)
    -- 0 1 2 3  4  5 6 7 8 9 10 11 12 13 14 15 16 17
    -- 2 3 2 x 2,3 x 2 3 2 x 2,3 x  2  3  2  x 2,3 x
    --     5           5            5              5
    -- 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 30
    -- 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2
    --   3 x x 3 x x 3 x x 3 x x 3 x x 3 x x 3 x x 3 x x 3 x x 3 x x 3
    --     5 x x x x 5 x x x x 5 x x x x 5 x x x x 5 x x x x 5 x x x x 5
    -- Should be 6, but I get 2 as an answer?
    --
    -- 0 1 2 3 4 5 6 7 8 9
    -- 2 x 2 x 2 x 2 x 2 x 2 x 2
    -- 3 x x 3 x x 3 x x 3 x x 3
    -- 5 x x x x 5 x x x x 5
    -- Answer: 8
    -- OHHHHH it works like that.  It doesn't mean busses depart starting
    -- at that offset, it means busses all start at 0 and when do they
    -- reach those offsets
    --
    -- Thanks to this realization, I should now be able to play around with
    -- combining easy numbers like 2 3 and 5 and see what I can predict
    --
    -- So, when talking about 2 and 3, it really makes sense that the first
    -- meeting is at time 2, and every meeting after that is a multiple of 6
    -- offset by 2.  To add on 5, it really must be the case that it needs to
    -- still conform to existing limitations, so it must be some number in the
    -- set of [2, 8, 14, ..], and this is correct, as it happens to be 8.  This
    -- means I'm definitely on the right track, I just need to figure out the
    -- right way to combine numbers.  It is my belief that the offset is what
    -- is messing this up.
    --
    -- Now say we combine 2 (offset 0) and 3 (offset 1) and find we get 6 with
    -- offset of 2.  I think I realize what the issue is.  This was my original,
    -- but incorrect understanding of what's going on.  See, 6 with an offset
    -- of 2 would work like...
    -- 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4
    -- x x 6 x x x x x 6 x x x x x 6
    -- But this is incorrect.  All busses start at 0, so it's not correct to
    -- call this a proper offset in the same way that I was using it before.
    --
    -- 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4
    -- x x 6 x x x x x 6 x x x x x 6
    -- 5 x x x x 5 x x x x 5
    --                 ^ ^ ^
    --                 2 places after the recurring 6
    -- The first number's offset doesn't really seem to be a proper offset.
    -- Instead, that's where it "starts" in the bus timing, and we want the
    -- second number to come <second number's offset> places after the first
    -- number, which in this case happens at time 8.
    --
    -- What happens if we subtract out the first number's offset and add it to
    -- the second number's offset?
    --
    -- 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4
    -- 6 x x x x x 6 x x x x x 6 x x
    -- 5 x x x x 5 x x x x 5
    --             ^ ^ ^ ^ ^
    --
    -- In this case, the recurring 6 would start at 0 like any normal bus, but
    -- we'd be looking for the second number to occur xoff+yoff after the
    -- first number, which puts it in the same place.  However, it looks like
    -- we have to add xoff to the timestamp found to produce 8.
    --
    --
    --
    -- (0,2) | (2, 3)
    --  0  1 2 3 4 5 6 7  8
    -- 2,3 x 2 3 2 x 3
    -- Answer: 4, makes sense now
    -- t0 = [Just 2, Nothing, Just 3]
    -- t0Ans = d13p2Go t0
    -- t0Ans2 = d13p2Go2 t0

    -- t1 = [Just 7, Just 13, Nothing, Nothing
    --   , Just 59, Nothing, Just 31, Just 19]
    -- t1Ans = d13p2Go t1

    -- t2 = [Just 17, Nothing, Just 13, Just 19]
    -- t2Ans = d13p2Go t2

    -- t3 = [Just 67, Just 7, Just 59, Just 61]
    -- t3Ans = d13p2Go t3

    -- t4 = [Just 67, Nothing, Just 7, Just 59, Just 61]
    -- t4Ans = d13p2Go t4

    -- t5 = [Just 67, Just 7, Nothing, Just 59, Just 61]
    -- t5Ans = d13p2Go t5

    -- t6 = [Just 67, Just 7]
    -- t6Ans = d13p2Go t6
    -- t6WOff = withOffsets t6
    -- t6Ans2 = foldl1' d13p2Combine t6WOff

    -- t7 = [Just 67, Just 7, Just 59]
    -- t7Ans = d13p2Go2 t7

    -- t8 = t3
    -- t8Ans = d13p2Go2 t8

    -- t90 = [Just 2, Just 3]
    -- t90Ans = d13p2Go t90
    -- t90Ans2 = d13p2Go2 t90

    -- t9 = [Just 2, Just 3, Just 5]
    -- t9Ans = d13p2Go t9
    -- t9Ans2 = d13p2Go2 t9

    -- t10 = [Just 2, Nothing, Just 3, Just 5]
    -- t10Ans = d13p2Go t10
    -- t10Ans2 = d13p2Go2 t10

    ans = d13p2Go2 m

  -- for_ withOffsets print
  -- print $ product nums
  -- print $ 1473355587699697 - 100000000000000
  -- print t0Ans
  -- print t1Ans
  -- print t2Ans
  -- print t3Ans
  -- print t4Ans
  -- print t5Ans
  -- print t6Ans
  -- print $ take 5 $ d13p2Test t6
  -- print t6Ans2
  -- print t7Ans
  -- print t8Ans
  -- print t0Ans2
  -- print t90Ans
  -- print t90Ans2
  -- print $ take 10 $ d13p2Test t90
  -- print t9Ans
  -- print t9Ans2
  -- print t10Ans
  -- print t10Ans2
  print ans

d14ParseInstr :: Text -> (Int, Int)
d14ParseInstr t = t
  & T.drop 4 -- drop "mem["
  & T.span (/= ']')
  & _1 %~ tToI
  & _2 %~ T.drop 4 -- drop "] = "
  & _2 %~ tToI

data D14p1MaskAction = SetZero Int | SetOne Int

d14p1ExecuteBatch :: forall sig m.
  Has (State (Map Int Int)) sig m => ([Char], [(Int, Int)]) -> m ()
d14p1ExecuteBatch (mask, instrs) = do
  -- mem <- get @(Map Int Int)
  let
    maskActions :: [D14p1MaskAction]
    maskActions = foldl' (\l (i, b) -> case b of
      '0' -> SetZero i : l
      '1' -> SetOne i : l
      'X' -> l
      _ -> error "Invalid mask"
      ) [] (zip [0..] mask)

    execute :: (Int, Int) -> m ()
    execute (i, val) = do
      let
        maskedVal :: Int
        maskedVal = foldl' (\x -> \case
          SetZero b -> clearBit x b
          SetOne b -> setBit x b
          ) val maskActions

      modify (M.insert i maskedVal)

  for_ instrs execute

d14p1 :: IO ()
d14p1 = do
  m :: [([Char], [(Int, Int)])] <- T.readFile "data/d14p1.txt"
    <&> T.splitOn "mask = "
    <&> tail
    <&> map (T.lines
      >>> splitAt 1
      >>> _1 %~ (head >>> T.reverse >>> T.unpack)
      >>> _2 %~ map d14ParseInstr
      )

  -- m :: [([Char], [(Int, Int)])] <- T.readFile "test/d14p1.txt"
  --   <&> T.splitOn "mask = "
  --   <&> tail
  --   <&> map (T.lines
  --     >>> splitAt 1
  --     >>> _1 %~ (head >>> T.reverse >>> T.unpack)
  --     >>> _2 %~ map d14ParseInstr
  --     )

  mem :: Map Int Int <- for_ m d14p1ExecuteBatch & execState M.empty
  let ans = M.foldr (+) 0 mem

  print ans

-- 0 and 5 are floating bits
-- setBit 0 AND setBit 5
-- clearBit 0 AND clearBit 5
-- setBit 5 AND clearBit 0
-- clearBit 5 AND setBit 0
--
-- 0 AND not 0
-- 5 AND not 5
-- all possible combinations
--
-- given an x from the list, x is with all OTHER items in the list AND x is
-- not with all OTHER items in the list
-- given l = [0, 5]
-- given x = 0
-- [[], [0], [0, 5]]
-- given x = 5
-- [[], [5], [0, 5]]
-- combine:
-- [[], [0], [0, 5], [5], [0,5], [0,5], [0,5], [0,5]]
-- remove duplicates;
-- [[], [0], [0, 5], [5]]
-- This is what I want, but how to achieve? Hm.
-- OH.  This is just subsequences.  Omegalul.
--
-- The way this relates to my problem is: I'll have some sort of "Floating bit"
-- action, similar to my actions from part 1, and I need the subsequences of
-- all of these actions.  Then I simply create a memory address for each of
-- these lists.
-- So, with 1st example
-- address: 42 = 101010
-- mask:         X1001X
-- result:       X1101X
--
-- This would generate an action list of
-- l = [Floating 0, Floating 5]
-- subsequences l = [[], [Floating 0], [Floating 5], [Floating 0, Floating 5]]
--
-- So, looking at this, it looks like I can have the result default to X = 0,
-- so a result of X1101X like in the example would be 011010 by default, and
-- then rather than "Floating bit actions", I can do a subsequence of just the
-- indecies [0,5] and then map (setBit result) over the result.
-- That should give me:
-- (setBit result) <$> [[], [0], [5], [0, 5]] (not quite)
-- setBit result [] = no actions (just result)
-- setBit result [0] = setBit result 0
-- setBit result [5] = setBit result 5
-- setBit result [0, 5] = setBit (setBit result 0) 5
--   = (flip setBit 5 . flip setBit 0)
--
-- I think this might be:
-- fmap (foldl' setBit result) (subsequences floatingIndices)
--
-- For the example, this would resolve to:
-- foldl' setBit result [] = result
-- foldl' setBit result [0] = setBit result 0
-- foldl' setBit result [5] = setBit result 5
-- foldl' setBit result [0,5] = foldl' setBit (setBit result 0) [5]
--   = setBit (setBit result 0) 5
-- This seems like it works
--
-- Note that "result" is the modified original address where X defaults to 0,
-- 0 means no change, and 1 sets it to 1.  I think the best way to handle this
-- is to note indecies of the Xs and save them to use later, then change all Xs
-- to 0s and simply `or` the mask with the original address, which I believe
-- should produce the desired result.
--
-- Okay, I am having a problem, and I definitely see something that is wrong.
-- I am doing what I suggested above: note the location of Xs and then replace
-- them with 0s and `or` with the original address.  The problem is that, while
-- I SHOULD be treating any floating point value as 0 by default, if it was 1
-- in the original address, I never clear it because 1 `or` 0 is 1.  I need to
-- fix this.  Boom.  Fixed it with
-- foldl' clearBit originalResult floatingIndices

-- goal: include x both with and without every other subsequence
-- I think I did this before btw.
mySubseq :: [a] -> [[a]]
mySubseq [] = [[]]
mySubseq (x:xs) = mySubseq xs ++ map (x:) (mySubseq xs)

readBinMaybe :: Integral a => [Char] -> Maybe a
readBinMaybe = fmap fst . listToMaybe . readInt 2 (`elem` ['0','1']) digitToInt

d14p2ExecuteBatch :: forall sig m.
  Has (State (Map Int Int)) sig m => ([Char], [(Int, Int)]) -> m ()
d14p2ExecuteBatch (mask, instrs) = do
  let
    floatingIndices :: [Int]
    floatingIndices = findIndices (=='X') mask

    defaultMask :: Int
    defaultMask = mask
      & reverse
      & map (\case 'X' -> '0'; c -> c)
      & readBinMaybe
      & fromJust

    execute :: (Int, Int) -> m ()
    execute (addr, val) = do
      let
        result :: Int
        result = foldl' clearBit (addr .|. defaultMask) (floatingIndices)

        addresses :: [Int]
        addresses = fmap (foldl' setBit result) (subsequences floatingIndices)

      for_ addresses \i -> modify (M.insert i val)

  for_ instrs execute

d14p2 :: IO ()
d14p2 = do
  m :: [([Char], [(Int, Int)])] <- T.readFile "data/d14p1.txt"
    <&> T.splitOn "mask = "
    <&> tail
    <&> map (T.lines
      >>> splitAt 1
      >>> _1 %~ (head >>> T.reverse >>> T.unpack)
      >>> _2 %~ map d14ParseInstr
      )

  mem :: Map Int Int <- for_ m d14p2ExecuteBatch & execState M.empty
  let ans = M.foldr (+) 0 mem

  print ans

d14p2MVectorExecuteBatch
  :: forall s. MVector s Int -> ([Char], [(Int, Int)]) -> ST s ()
d14p2MVectorExecuteBatch vec (mask, instrs) = do
  let
    floatingIndices :: [Int]
    floatingIndices = findIndices (=='X') mask

    defaultMask :: Int
    defaultMask = mask
      & reverse
      & map (\case 'X' -> '0'; c -> c)
      & readBinMaybe
      & fromJust

    execute :: (Int, Int) -> ST s ()
    execute (addr, val) = do
      let
        result :: Int
        result = foldl' clearBit (addr .|. defaultMask) (floatingIndices)

        addresses :: [Int]
        addresses = fmap (foldl' setBit result) (subsequences floatingIndices)

      for_ addresses \i -> MV.write vec i val

  for_ instrs execute

-- same thing as the regular solution, but I decided to play around with
-- MVector and ST s, which has been historically confusing, instead of fused
-- effects state with a Map
--
-- This works as in it can do the small example and get the correct answer, but
-- I can't make the vector big enough to hold the memory locations from the
-- actual data set.  A vector simply isn't suitable to this problem.
d14p2MVector :: IO ()
d14p2MVector = do
  m :: [([Char], [(Int, Int)])] <- T.readFile "test/d14p2.txt"
    <&> T.splitOn "mask = "
    <&> tail
    <&> map (T.lines
      >>> splitAt 1
      >>> _1 %~ (head >>> T.reverse >>> T.unpack)
      >>> _2 %~ map d14ParseInstr
      )

  let
    vec :: Vector Int
    vec = runST do
      v <- MV.replicate 100000 0
      for_ m (d14p2MVectorExecuteBatch v)
      v' <- V.freeze v
      pure v'

    ans = V.sum vec

  print ans

-- Map index num -> result of 2020th num
d15p1Go :: Map Int Int -> Int -> Int -> Int
d15p1Go _ 2020 n = n
d15p1Go m i n = d15p1Go m' (i+1) next
  where
    next = case m !? n of
      Nothing -> 0
      Just prevSpoken -> i - prevSpoken
    m' = M.insert n i m

-- preamble
-- 12 - 1
-- 1 - 2
-- 16 - 3
-- 3 - 4
-- 11 - 5
-- 0 - 6
--
-- now go:
-- 0 - 7 last time spoken 7 - 6 = 1
-- 1 - 8 last time spoken 8 - 2 = 6
-- 6 - 9 not spoken
-- 0 - 10 last time spoken 10 - 7 = 3
--
-- So I need to keep track of the current index and current number about to
-- be spoken; use that number to consider next number and increment index by 1.
-- When a number is newly spoken, remember it in the Map, and replace previous
-- entries.  When a number is found to have been spoken before, the next number
-- is current index - value in Map corresponding to that number (before replace)
d15p1 :: IO ()
d15p1 = do
  let
    input :: [Int]
    input = [12,1,16,3,11,0]

    initialMap :: Map Int Int
    initialMap = M.fromList $ zip input [1..]

    initialIndex = length input + 1

    initialNum = 0

    ans = d15p1Go initialMap initialIndex initialNum

  print ans

-- Surprisingly, this function was actually kind of slow, but it was able to
-- compute the answer after about minute.  Well within reason
-- to brute force it, but I honestly didn't expect 30,000,000 iterations to
-- take all that long.  I shouldn't be doing anything more than O(log n) per
-- iteration.
--
-- Map index num -> result of 30000000th num
d15p2Go :: Map Int Int -> Int -> Int -> Int
d15p2Go _ 30000000 n = n
d15p2Go m i n = d15p2Go m' (i+1) next
  where
    next = case m !? n of
      Nothing -> 0
      Just prevSpoken -> i - prevSpoken
    m' = M.insert n i m

d15p2 :: IO ()
d15p2 = do
  let
    input :: [Int]
    input = [12,1,16,3,11,0]

    initialMap :: Map Int Int
    initialMap = M.fromList $ zip input [1..]

    initialIndex = length input + 1

    initialNum = 0

    ans = d15p2Go initialMap initialIndex initialNum

  print ans

d16Parse :: Text -> ([(Text, ((Int, Int), (Int, Int)))], ([Int], [[Int]]))
d16Parse = T.lines
  >>> splitAt 20
  >>> _1 %~ map (T.splitOn ": " >>> splitAt 1 >>> both %~ head)
  >>> _1 %~ map (_2 %~ (T.splitOn " or " >>> splitAt 1 >>> both %~ head))
  >>> _1 %~ map (_2.both %~ (T.split (=='-')
    >>> splitAt 1
    >>> both %~ (head >>> tToI)
    ))
  >>> _2 %~ (drop 2 >>> splitAt 1)
  >>> _2._1 %~ (head
    >>> T.split (==',') >>> map tToI)
  >>> _2._2 %~ (drop 2
    >>> map (T.split (==',') >>> map tToI))

d16ParseRangesAll :: [(Text, ((Int, Int), (Int, Int)))] -> Set Int
d16ParseRangesAll = foldr f S.empty
  where
    f (_, ((a, b), (c, d))) s =
      S.union s (S.union (S.fromList [a..b]) (S.fromList[c..d]))

d16p1 :: IO ()
d16p1 = do
  m <- T.readFile "data/d16p1.txt"
    <&> d16Parse

  let
    (fields, (myTicket, otherTickets)) = m
    ranges = d16ParseRangesAll fields
    bads = foldr f [] otherTickets
      where
        f ticket bs = filter (`S.notMember` ranges) ticket ++ bs
    ans = sum bads

  print ranges
  print myTicket
  print bads
  print ans

d16p2ParseRanges :: [(Text, ((Int, Int), (Int, Int)))] -> Map Text (Set Int)
d16p2ParseRanges = M.fromList . map (_2 %~ \((a, b), (c, d)) ->
  S.union (S.fromList [a..b]) (S.fromList[c..d]))

d16p2Go :: [Vector Int] -> Map Text (Set Int) -> Map Text Int
d16p2Go tickets ranges =
  go (Seq.fromList (M.keys ranges)) initPossibleLoc M.empty
  where
    go :: Seq Text -> Map Text (Set Int) -> Map Text Int -> Map Text Int
    go Seq.Empty _ finalLoc = finalLoc
    go (field :<| q) possibleLoc finalLoc = if S.size locs' == 1
      then
        let
          loc = S.elemAt 0 locs'
          possibleLoc' = M.map (S.delete loc) (M.delete field possibleLoc)
          finalLoc' = M.insert field loc finalLoc
        in go q possibleLoc' finalLoc'
      else go (q :|> field) possibleLoc finalLoc
      where
        locs = case possibleLoc !? field of
          Nothing -> error "invalid field in queue"
          Just ls -> ls

        -- test i against all vectors in tickets; if any fail, remove i from
        -- ls
        locs' = S.foldl' (\ls i -> if any (\v ->
            S.notMember (v ! i) (ranges M.! field)
          ) tickets
          then S.delete i ls
          else ls
          ) locs locs

    initPossibleLoc =
      M.fromList $ zip (M.keys ranges) (repeat $ S.fromList [0..19])

-- My plan is to basically have some sort of queue that keeps track of the
-- fields that have yet to find their spot.  I will have a Map of the possible
-- locations that a field can still go.  When a field's possible locations is
-- narrowed down to 1, then that is the location for that field, and I will
-- remove it from the queue, and also remove that location as a possible
-- location from all other fields.  I might also put that field into a
-- separate Map that keeps track of the final locations for the fields for
-- convenience, so I don't have to worry about the field's possible location
-- being preserved in the Map that keeps track of field's possible locations.
--
-- When it is a field's turn in the queue, first it will check to see if it
-- has been narrowed down to a single location yet.  If so, it will do what
-- I said previously and then move to the next field until the queue is empty.
-- If it is not yet narrowed down, it will check the locations of each ticket.
-- If it finds an invalidation, then it will remove that location from its
-- list of possible locations.  Because I need to keep track of the indices
-- of each ticket, I will likely want the tickets to be Vector Int.  After
-- it is finished narrowing down its possible locations, it will check again
-- to see if it has been narrowed down to a single location.  If so, it does
-- the stuff I mentioned previously.  If not, then it's not possible to narrow
-- this field to 1 location yet, so it will put it at the end of the queue and
-- move to the next entry.  The idea is to continue doing this until the queue
-- is empty, which must happen at some point.  I believe the queue will be a
-- Sequence.
d16p2 :: IO ()
d16p2 = do
  m <- T.readFile "data/d16p1.txt"
    <&> d16Parse

  let
    (fields, (myTicket, otherTickets)) = m
    allRanges = d16ParseRangesAll fields
    ranges = d16p2ParseRanges fields
    validTickets = myTicket : filter (all (`S.member` allRanges)) otherTickets

    finalLocations = d16p2Go (map V.fromList validTickets) ranges
    myTicketV = V.fromList myTicket
    is = M.elems $ M.filterWithKey
      (\t _ -> T.isPrefixOf "departure" t)
      finalLocations
    departureVals = map (myTicketV !) is
    ans = product departureVals

  print myTicket
  print finalLocations
  print departureVals
  print ans

d17p1Neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
d17p1Neighbors (x, y, z) =
  [ (a,b,c)
  | a <- [x-1 .. x+1]
  , b <- [y-1 .. y+1]
  , c <- [z-1 .. z+1]
  , a /= x || b /= y || c /= z
  ]

d17p1Cycle :: Set (Int, Int, Int) -> Set (Int, Int, Int)
d17p1Cycle active = active'
  where
    tally :: Map (Int, Int, Int) Int
    tally = S.foldr (\cube m ->
      foldr (\neighbor -> M.insertWith (+) neighbor 1) m (d17p1Neighbors cube))
      M.empty active

    tally' :: Map (Int, Int, Int) Int
    tally' = M.filter (\x -> x == 2 || x == 3) tally

    candidates :: [(Int, Int, Int)]
    candidates = M.keys tally'

    active' :: Set (Int, Int, Int)
    active' = S.fromList $ filter
      (\cube -> S.member cube active || tally' M.! cube == 3)
      candidates

-- This seems hard because trying to brute force it probably isn't possible.
-- Each cycle multiplies the cube size by 26, so that would reach over 10
-- billion.  That's not a number that is out of this world huge, but it's
-- probably big enough where I won't be able to simply do it the naive way.
--
-- My plan at the moment is to keep track of "cubes I'm interested in", and
-- I think I might have something like a tally Map to keep track of the number
-- of times a cube is "touched" by active cubes.  If a cube is "touched" by
-- 2 or 3 active cubes, I'll look closer and determine whether it should
-- turn on, remain on, turn off, or remain off.  If a cube is not "touched"
-- by exactly 2 or 3 active cubes, then it should be turned off, regardless
-- of its previous state.
--
-- So I believe that I will maintain a Set of active cubes and a tally Map
-- of how many times an active cube has touched that position during the
-- cycle.  Each cycle, I will iterate through the list of active cubes and
-- have each of them update the tally Map based on which of their neighbors
-- that they touched.  Since the number of active cubes should be far smaller
-- than the total number of cubes in the 3D space that I could potentially
-- touch, this should not be too much iteration.  Then, after I am finished
-- updating the tally Map for the cycle, I check the Tally map and update the
-- cube states according to the rules.  If a cube is active and has 2 or 3
-- cubes in its tally, then it stays on (I'd probably do this by simply making
-- a new "active cubes" Set and adding the cube to it).  If a cube is inactive
-- and has exactly 3 cubes in its tally, turn it on (add it to the Set).  All
-- other cubes would turn off or remain off (simply don't add them to the new
-- Set).  Each cycle should start with a new "active cubes" Set and an empty
-- tally Map.
--
-- A note about iterate because I had originally gotten the wrong answer
-- due to my unthinking misinterpretation of it: the first elemnent of iterate
-- (index 0) is zero iterations.  The second element (index 1) is the 1st
-- iteration, and so on.  I had originally thought that cycle 6 would be
-- element 6 (index 5), but it is actually element 7 (index 6).
d17p1 :: IO ()
d17p1 = do
  active :: Set (Int, Int, Int) <- T.readFile "data/d17p1.txt"
    <&> T.lines
    <&> map T.unpack
    <&> map (filter (\(_,c) -> c=='#') . zip [0..])
    <&> zip [0..]
    <&> concatMap (\(y, xs) -> map (\(x, _) -> (x, y, 0)) xs)
    <&> S.fromList

  let
    sixthCycleActive :: Set (Int, Int, Int)
    sixthCycleActive = iterate d17p1Cycle active !! 6

    ans = S.size sixthCycleActive

    foo = take 10 $ iterate d17p1Cycle active
    boo = map S.size foo

  print active
  print ans
  print boo

d17p2Neighbors :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
d17p2Neighbors (x, y, z, w) =
  [ (a,b,c,d)
  | a <- [x-1 .. x+1]
  , b <- [y-1 .. y+1]
  , c <- [z-1 .. z+1]
  , d <- [w-1 .. w+1]
  , a /= x || b /= y || c /= z || d /= w
  ]

d17p2Cycle :: Set (Int, Int, Int, Int) -> Set (Int, Int, Int, Int)
d17p2Cycle active = active'
  where
    tally :: Map (Int, Int, Int, Int) Int
    tally = S.foldr (\cube m ->
      foldr (\neighbor -> M.insertWith (+) neighbor 1) m (d17p2Neighbors cube))
      M.empty active

    tally' :: Map (Int, Int, Int, Int) Int
    tally' = M.filter (\x -> x == 2 || x == 3) tally

    candidates :: [(Int, Int, Int, Int)]
    candidates = M.keys tally'

    active' :: Set (Int, Int, Int, Int)
    active' = S.fromList $ filter
      (\cube -> S.member cube active || tally' M.! cube == 3)
      candidates

d17p2 :: IO ()
d17p2 = do
  active :: Set (Int, Int, Int, Int) <- T.readFile "data/d17p1.txt"
    <&> T.lines
    <&> map T.unpack
    <&> map (filter (\(_,c) -> c=='#') . zip [0..])
    <&> zip [0..]
    <&> concatMap (\(y, xs) -> map (\(x, _) -> (x, y, 0, 0)) xs)
    <&> S.fromList

  let
    -- HEY FUCK YOU FUCK ASS MULTIPLE CURSORS
    sixthCycleActive :: Set (Int, Int, Int, Int)
    sixthCycleActive = iterate d17p2Cycle active !! 6

    ans = S.size sixthCycleActive

    foo = take 10 $ iterate d17p2Cycle active
    boo = map S.size foo

  print active
  print ans
  print boo
