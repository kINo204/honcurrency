{- HLINT ignore "Redundant pure" -}
module Instr
  ( Instr (..),
    Operator (..),
    Operand,
    runInstr,
  )
where

import Machine

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

type Operand = Int -- TODO: Nil operand

data Instr = Instr Operator Operand Operand

runInstr :: Instr -> Frame -> Execution Frame
runInstr (Instr op a b) f =
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
      m <- readMem a
      f <- writeRegM b m f
      f <- mapPcM (+ 1) f
      pure f
    Sto -> do
      x <- readRegM a f
      writeMem b x
      f <- mapPcM (+ 1) f
      pure f
    _ -> pure f
