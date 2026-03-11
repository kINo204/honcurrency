module Core.Machine
  ( Frame (..), Machine (..), SymTbl,
    frame, machine,
    Execution (..),
    setPcM, mapPcM, readRegM, writeRegM, readMem, writeMem, findsym,
  )
where

import Control.Monad (ap, liftM)
import Data.Array (Array, array, (!), (//))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

---------------------------------------------------------------------

type Mem = Array Int Int
type SymTbl = Map.Map String Int

data Frame = Frame
  { pc :: Int,
    regs :: Mem
  }
  deriving (Show)

data Machine = Machine
  { mem :: Mem,
    symtbls :: [SymTbl]
  }
  deriving (Show)

memory :: Int -> Mem
memory size = array (0, size - 1) [(i, 0) | i <- [0 .. size - 1]]

frame :: Int -> Frame
frame nregs = Frame 0 (memory nregs)

machine :: Int -> [SymTbl] -> Machine
machine sizeMem = Machine (memory sizeMem)

readReg :: Int -> Frame -> Int
readReg i frame = regs frame ! i

writeReg :: Int -> Int -> Frame -> Frame
writeReg i x frame =
  Frame (pc frame) (regs frame // [(i, x)])

mapPc :: (Int -> Int) -> Frame -> Frame
mapPc g frame = Frame (g $ pc frame) (regs frame)

---------------------------------------------------------------------

newtype Execution a = Execution {execute :: Machine -> (Machine, a)}

instance Monad Execution where
  (>>=) :: Execution a -> (a -> Execution b) -> Execution b
  e0 >>= f =
    Execution $ \machine0 ->
      let (machine1, a1) = execute e0 machine0
          e1 = f a1
          (machine2, a2) = execute e1 machine1
       in (machine2, a2)

instance Applicative Execution where
  pure :: a -> Execution a
  pure a = Execution (,a)

  (<*>) :: Execution (a -> b) -> Execution a -> Execution b
  (<*>) = ap

instance Functor Execution where
  fmap = liftM

readRegM :: Int -> Frame -> Execution Int
readRegM i frame = pure $ readReg i frame

writeRegM :: Int -> Int -> Frame -> Execution Frame
writeRegM i a frame = pure $ writeReg i a frame

setPcM :: Int -> Frame -> Execution Frame
setPcM dest frame = pure $ Frame dest (regs frame)

mapPcM :: (Int -> Int) -> Frame -> Execution Frame
mapPcM g frame = pure $ mapPc g frame

readMem :: Int -> Execution Int
readMem i = Execution $ \machine ->
  let a = mem machine ! i in (machine, a)

writeMem :: Int -> Int -> Execution ()
writeMem i a = Execution $ \machine ->
  let m = mem machine // [(i, a)] in (Machine m (symtbls machine), ())

findsym :: Int -> String -> Execution Int
findsym thread_id label = Execution $ \machine ->
    let symtbl = symtbls machine !! thread_id
        lineno = fromMaybe (-1) $ Map.lookup label symtbl
     in (machine, lineno)
