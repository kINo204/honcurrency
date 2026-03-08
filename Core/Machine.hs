module Core.Machine
  ( Frame (..), Machine (..),
    frame, machine,
    Execution (..),
    mapPcM, readRegM, writeRegM, readMem, writeMem,
  )
where

import Control.Monad (ap, liftM)
import Data.Array (Array, array, (!), (//))

---------------------------------------------------------------------

type Mem = Array Int Int

data Frame = Frame
  { pc :: Int,
    regs :: Mem
  }
  deriving (Show)

data Machine = Machine {mem :: Mem} deriving (Show)

memory :: Int -> Mem
memory size = array (0, size - 1) [(i, 0) | i <- [0 .. size - 1]]

frame :: Int -> Frame
frame nregs = Frame 0 (memory nregs)

machine :: Int -> Machine
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
readRegM i frame =
  Execution $
    let a = readReg i frame in (,a)

writeRegM :: Int -> Int -> Frame -> Execution Frame
writeRegM i a frame =
  Execution $
    let f = writeReg i a frame in (,f)

mapPcM :: (Int -> Int) -> Frame -> Execution Frame
mapPcM g frame =
  Execution $
    let f = mapPc g frame in (,f)

readMem :: Int -> Execution Int
readMem i = Execution $ \machine ->
  let a = mem machine ! i in (machine, a)

writeMem :: Int -> Int -> Execution ()
writeMem i a = Execution $ \machine ->
  let m = mem machine // [(i, a)] in (Machine m, ())
