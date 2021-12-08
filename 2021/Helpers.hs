module Helpers where

import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Debug.Trace
import Numeric (readInt)

tToI :: Integral a => Text -> a
tToI x =
  T.signed T.decimal x
    & fromRight (trace (T.unpack x) undefined)
    & fst

tToIMaybe :: Integral a => Text -> Maybe a
tToIMaybe str = case T.decimal str of
  Left _ -> Nothing
  Right (x, _) -> Just x

readLines :: FilePath -> IO [Text]
readLines fp = T.readFile fp <&> T.lines

readNums :: FilePath -> IO [Int]
readNums fp = T.readFile fp <&> T.lines <&> fmap tToI

readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` ("01" :: String)) digitToInt
