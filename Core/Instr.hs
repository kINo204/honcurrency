{- HLINT ignore "Redundant pure" -}
module Core.Instr
  ( Instr (..),
    Operator (..),
    Operand (..),
    runInstr,
  )
where

import Control.Monad
import Core.Machine

data Operator
  = -- Arithmetic
    Add | Sub | Imm | Adi | Sbi |
  -- Memory Operations
    Lod | Sto | Ldr | Str |
  -- Branching & Control Flow
    Lab | Br | Btr | Bfs |
    -- Processor controlling
    Yld | Blk | Pst |
    -- Concurrency
    Cas |
    -- Debugging
    Prt | Prs | Err
  deriving (Show)

data Operand = Num Int | Msg String -- TODO: Nil operand
  deriving (Show)

data Instr = Instr Operator Operand Operand
  deriving (Show)

runInstr :: Instr -> Frame -> Execution Frame
runInstr (Instr op (Num a) (Num b)) f =
  case op of
    Add -> do
      x <- readRegM a f
      y <- readRegM b f
      f <- writeRegM a (x + y) f
      f <- mapPcM (+ 1) f
      pure f
    Imm -> do
      f <- writeRegM a b f
      f <- mapPcM (+ 1) f
      pure f
    Lod -> do
      m <- readMem b
      f <- writeRegM a m f
      f <- mapPcM (+ 1) f
      pure f
    Sto -> do
      x <- readRegM a f
      writeMem b x
      f <- mapPcM (+ 1) f
      pure f
    Cas -> do
      m <- readMem b
      f <- writeRegM a m f
      when (m == 0) $
        writeMem b 1
      f <- mapPcM (+ 1) f
      pure f
    Br -> do
      f <- mapPcM (+ a) f
      pure f
    Btr -> do
      x <- readRegM b f
      let dpc = if x /= 0 then a else 1
       in mapPcM (+ dpc) f
    Bfs -> do
      x <- readRegM b f
      let dpc = if x == 0 then a else 1
       in mapPcM (+ dpc) f
    _ -> pure f

runInstr (Instr op (Msg a) (Num b)) f =
  case op of
    _ -> pure f
