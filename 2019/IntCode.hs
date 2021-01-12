module IntCode where

import Data.Function ((&))
import Data.Vector.Unboxed ((!), (//), Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector, MVector)
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.Reader (ReaderT)
import Control.Monad.Writer.Strict (WriterT)
import Control.Monad.ST (ST)
import Control.Algebra
import Control.Carrier.State.Strict
import Control.Carrier.Writer.Strict
import Control.Effect.Writer
import Control.Effect.State
import Control.Effect.Reader
import Control.Effect.Error
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Data.Sequence (Seq ((:<|), (:|>)))

type Memory = IOVector Int

-- I want to play around with fused effects, perhaps
-- ...If I can figure out how to set it up
-- type SomeVec s = ReaderT (MVector s Int) (WriterT [String] (ST s))

someFunc :: forall sig m a.
  ( Has (State (IOVector Int)) sig m
  , Has (Writer [String]) sig m
  , Effect sig, MonadIO m )
  => m Int
someFunc = do
  vec <- get @(IOVector Int)
  cmd <- liftIO $ V.read vec 1
  undefined

something :: IO ()
something = do
  let
    initialValues :: Vector Int
    initialValues = V.fromList
      [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
  mem :: IOVector Int <- V.thaw initialValues
  (output, (mem', x)) :: ([String], (IOVector Int, Int))
    <- someFunc & runState mem & runWriter
  undefined


-- data MemEffect m k
-- newtype MemCarrier m a

-- instance
--   ( Algebra m sig
--   , Has (Reader Memory) sig m
--   , Has (State Memory) sig m
--   )
--   => Algebra (MemEffect :+: sig) (MemCarrier m)

-- MemCarrier (ReaderC (WriterC



-- This function should recursively parse the whole memory, computing the
-- final result
runIntCode
  :: forall sig m.
    ( Has (Reader Memory) sig m
    , Has (State (Seq Int)) sig m
    , Has (Error String) sig m
    , Effect sig, MonadIO m )
  => Int    -- index to read
  -> m ()
runIntCode i = do
  v <- ask @Memory
  cmd <- liftIO $ V.read v i
  let
    paramModes :: Int
    opcode :: Int
    (paramModes, opcode) = cmd `divMod` 100

    pMode1 :: Int
    pMode1 = paramModes `mod` 10

    pMode2 :: Int
    pMode2 = paramModes `div` 10 `mod` 10

    pMode3 :: Int
    pMode3 = paramModes `div` 100
  case opcode of
    1 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      liftIO $ V.write v z (x + y)
      runIntCode (i + 4)
    2 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      liftIO $ V.write v z (x * y)
      runIntCode (i + 4)
    3 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      ~(n :<| input') <- get @(Seq Int)
      liftIO $ V.write v x n
      put input'
      runIntCode (i + 2)
    4 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      n <- liftIO $ V.read v x
      -- print n
      modify (:|> n)
      runIntCode (i + 2)
    5 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      if x == 0
        then runIntCode (i + 3)
        else runIntCode y
    6 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      if x == 0
        then runIntCode y
        else runIntCode (i + 3)
    7 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      if x < y
        then liftIO $ V.write v z 1
        else liftIO $ V.write v z 0
      runIntCode (i + 4)
    8 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      if x == y
        then liftIO $ V.write v z 1
        else liftIO $ V.write v z 0
      runIntCode (i + 4)
    99 -> pure ()
    _ -> throwError @String "Input was bad or machine failed"

















-- This function should recursively parse the whole memory, computing the
-- final result
runIntCodeLegacy3
  :: forall sig m.
    ( Has (Reader Memory) sig m
    , Has (State [Int]) sig m
    , Has (Writer [Int]) sig m
    , Has (Error String) sig m
    , Effect sig, MonadIO m )
  => Int    -- index to read
  -> m ()
runIntCodeLegacy3 i = do
  v <- ask @Memory
  cmd <- liftIO $ V.read v i
  let
    paramModes :: Int
    opcode :: Int
    (paramModes, opcode) = cmd `divMod` 100

    pMode1 :: Int
    pMode1 = paramModes `mod` 10

    pMode2 :: Int
    pMode2 = paramModes `div` 10 `mod` 10

    pMode3 :: Int
    pMode3 = paramModes `div` 100
  case opcode of
    1 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      liftIO $ V.write v z (x + y)
      runIntCodeLegacy3 (i + 4)
    2 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      liftIO $ V.write v z (x * y)
      runIntCodeLegacy3 (i + 4)
    3 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      ~(n:input') <- get @[Int]
      liftIO $ V.write v x n
      put input'
      runIntCodeLegacy3 (i + 2)
    4 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      n <- liftIO $ V.read v x
      -- print n
      tell [n]
      runIntCodeLegacy3 (i + 2)
    5 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      if x == 0
        then runIntCodeLegacy3 (i + 3)
        else runIntCodeLegacy3 y
    6 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      if x == 0
        then runIntCodeLegacy3 y
        else runIntCodeLegacy3 (i + 3)
    7 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      if x < y
        then liftIO $ V.write v z 1
        else liftIO $ V.write v z 0
      runIntCodeLegacy3 (i + 4)
    8 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- liftIO $ V.read v xii
      yi <- liftIO $ V.read v yii
      x <- case pMode1 of
        0 -> liftIO $ V.read v xi
        1 -> pure xi
        _ -> throwError @String "bad param mode"
      y <- case pMode2 of
        0 -> liftIO $ V.read v yi
        1 -> pure yi
        _ -> throwError @String "bad param mode"
      z <- case pMode3 of
        0 -> liftIO $ V.read v zi
        1 -> pure zi
        _ -> throwError @String "bad param mode"
      if x == y
        then liftIO $ V.write v z 1
        else liftIO $ V.write v z 0
      runIntCodeLegacy3 (i + 4)
    99 -> pure ()
    _ -> throwError @String "Input was bad or machine failed"














-- This function should recursively parse the whole memory, computing the
-- final result
runIntCodeLegacy2
  :: [Int]       -- input
  -> [Int]       -- output
  -> Int         -- index to read
  -> Memory      -- initial state
  -> IO Memory      -- resulting state
runIntCodeLegacy2 input output i v = do
  cmd <- V.read v i
  let
    paramModes :: Int
    opcode :: Int
    (paramModes, opcode) = cmd `divMod` 100

    pMode1 :: Int
    pMode1 = paramModes `mod` 10

    pMode2 :: Int
    pMode2 = paramModes `div` 10 `mod` 10

    pMode3 :: Int
    pMode3 = paramModes `div` 100
  case opcode of
    1 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      V.write v z (x + y)
      runIntCodeLegacy2 input output (i + 4) v
    2 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      V.write v z (x * y)
      runIntCodeLegacy2 input output (i + 4) v
    3 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      let
        n = head input
        input' = tail input
      -- n :: Int <- read <$> getLine
      V.write v x n
      -- V.freeze v >>= print
      runIntCodeLegacy2 input' output (i + 2) v
    4 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      n <- V.read v x
      print n
      runIntCodeLegacy2 input (n:output) (i + 2) v
    5 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      if x == 0
        then runIntCodeLegacy2 input output (i + 3) v
        else runIntCodeLegacy2 input output y v
    6 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      if x == 0
        then runIntCodeLegacy2 input output y v
        else runIntCodeLegacy2 input output (i + 3) v
    7 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      if x < y then V.write v z 1 else V.write v z 0
      runIntCodeLegacy2 input output (i + 4) v
    8 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      if x == y then V.write v z 1 else V.write v z 0
      runIntCodeLegacy2 input output (i + 4) v
    99 -> pure v
    _ -> error "Input was bad or machine failed"

-- This function should recursively parse the whole memory, computing the
-- final result
runIntCodeLegacy
  :: Int         -- index to read
  -> Memory      -- initial state
  -> IO Memory      -- resulting state
runIntCodeLegacy i v = do
  cmd <- V.read v i
  let
    paramModes :: Int
    opcode :: Int
    (paramModes, opcode) = cmd `divMod` 100

    pMode1 :: Int
    pMode1 = paramModes `mod` 10

    pMode2 :: Int
    pMode2 = paramModes `div` 10 `mod` 10

    pMode3 :: Int
    pMode3 = paramModes `div` 100
  case opcode of
    1 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      V.write v z (x + y)
      runIntCodeLegacy (i + 4) v
    2 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      V.write v z (x * y)
      runIntCodeLegacy (i + 4) v
    3 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      n :: Int <- read <$> getLine
      V.write v x n
      runIntCodeLegacy (i + 2) v
    4 -> do
      let
        xi = i + 1
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      n <- V.read v x
      print n
      runIntCodeLegacy (i + 2) v
    5 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      if x == 0 then runIntCodeLegacy (i + 3) v else runIntCodeLegacy y v
    6 -> do
      let
        xii = i + 1
        yii = i + 2
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      if x == 0 then runIntCodeLegacy y v else runIntCodeLegacy (i + 3) v
    7 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      if x < y then V.write v z 1 else V.write v z 0
      runIntCodeLegacy (i + 4) v
    8 -> do
      let
        xii = i + 1
        yii = i + 2
        zi = i + 3
      xi <- V.read v xii
      yi <- V.read v yii
      x <- case pMode1 of
        0 -> V.read v xi
        1 -> pure xi
        _ -> error "bad param mode"
      y <- case pMode2 of
        0 -> V.read v yi
        1 -> pure yi
        _ -> error "bad param mode"
      z <- case pMode3 of
        0 -> V.read v zi
        1 -> pure zi
        _ -> error "bad param mode"
      if x == y then V.write v z 1 else V.write v z 0
      runIntCodeLegacy (i + 4) v
    99 -> pure v
    _ -> error "Input was bad or machine failed"
