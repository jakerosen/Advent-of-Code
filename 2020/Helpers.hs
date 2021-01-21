module Helpers where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Function ((&))
import Data.Either (fromRight)
import Debug.Trace

tToI :: Integral a => Text -> a
tToI x = T.signed T.decimal x
  & fromRight (trace (T.unpack x) undefined)
  & fst

tToIMaybe :: Integral a => Text -> Maybe a
tToIMaybe str = case T.decimal str of
  Left _ -> Nothing
  Right (x, _) -> Just x
