module Core.Instr
  ( Instr (..),
    Operator (..),
    Operand (..),
    SymTbl,
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
    Lab | Br | Btr | Bfs | Bgz | Blz | Bgez | Blez |
    -- Processor controlling
    Yld | Blk | Pst | Tid |
    -- Concurrency
    Cas |
    -- Debugging
    Prt | Prs | Err
  deriving (Show)

data Operand = Num Int | Msg String -- TODO: Nil operand
  deriving (Show)

data Instr = Instr Operator Operand Operand
  deriving (Show)

runInstr :: Instr -> Int -> Frame -> Execution Frame
runInstr (Instr op (Num a) (Num b)) tid f =
  case op of
    Add -> do
      x <- readRegM a f
      y <- readRegM b f
      f <- writeRegM a (x + y) f
      mapPcM (+ 1) f
    Sub -> do
      x <- readRegM a f
      y <- readRegM b f
      f <- writeRegM a (x - y) f
      mapPcM (+ 1) f
    Imm -> do
      f <- writeRegM a b f
      mapPcM (+ 1) f
    Adi -> do
      x <- readRegM a f
      f <- writeRegM a (x + b) f
      mapPcM (+ 1) f
    Sbi -> do
      x <- readRegM a f
      f <- writeRegM a (x - b) f
      mapPcM (+ 1) f
    Lod -> do
      m <- readMem b
      f <- writeRegM a m f
      mapPcM (+ 1) f
    Sto -> do
      x <- readRegM a f
      writeMem b x
      mapPcM (+ 1) f
    Ldr -> do
      ma <- readRegM b f
      m <- readMem ma
      f <- writeRegM a m f
      mapPcM (+ 1) f
    Str -> do
      ma <- readRegM b f
      x <- readRegM a f
      writeMem ma x
      mapPcM (+ 1) f
    Cas -> do
      m <- readMem b
      f <- writeRegM a m f
      when (m == 0) $
        writeMem b 1
      mapPcM (+ 1) f
    Br -> do
      mapPcM (+ a) f
    Btr -> do
      x <- readRegM b f
      let dpc = if x /= 0 then a else 1
       in mapPcM (+ dpc) f
    Bfs -> do
      x <- readRegM b f
      let dpc = if x == 0 then a else 1
       in mapPcM (+ dpc) f
    Bgz -> do
      x <- readRegM b f
      let dpc = if x > 0 then a else 1
       in mapPcM (+ dpc) f
    Blz -> do
      x <- readRegM b f
      let dpc = if x < 0 then a else 1
       in mapPcM (+ dpc) f
    Bgez -> do
      x <- readRegM b f
      let dpc = if x >= 0 then a else 1
       in mapPcM (+ dpc) f
    Blez -> do
      x <- readRegM b f
      let dpc = if x <= 0 then a else 1
       in mapPcM (+ dpc) f
    Blk -> do
      block tid
      mapPcM (+ 1) f
    Pst -> do
      x <- readRegM a f
      post x
      mapPcM (+ 1) f
    Tid -> do
      f <- writeRegM a tid f
      mapPcM (+ 1) f
    _ -> mapPcM (+ 1) f

runInstr (Instr op (Msg a) (Num b)) tid f =
  case op of
    Br -> do
      d <- findsym tid a
      setPcM d f
    Btr -> do
      x <- readRegM b f
      if x /= 0
        then do
          d <- findsym tid a
          setPcM d f
        else mapPcM (+ 1) f
    Bfs -> do
      x <- readRegM b f
      if x == 0
        then do
          d <- findsym tid a
          setPcM d f
        else mapPcM (+ 1) f
    Bgz -> do
      x <- readRegM b f
      if x > 0
        then do
          d <- findsym tid a
          setPcM d f
        else mapPcM (+ 1) f
    Blz -> do
      x <- readRegM b f
      if x < 0
        then do
          d <- findsym tid a
          setPcM d f
        else mapPcM (+ 1) f
    Bgez -> do
      x <- readRegM b f
      if x >= 0
        then do
          d <- findsym tid a
          setPcM d f
        else mapPcM (+ 1) f
    Blez -> do
      x <- readRegM b f
      if x <= 0
        then do
          d <- findsym tid a
          setPcM d f
        else mapPcM (+ 1) f
    _ -> mapPcM (+ 1) f
