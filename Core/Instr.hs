{- HLINT ignore "Redundant pure" -}
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
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

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

type SymTbl = Map.Map String Int

findsym :: SymTbl -> String -> Execution Int
findsym symtbl label =
  pure $
    fromMaybe (-1) $
      Map.lookup label symtbl

runInstr :: Instr -> SymTbl -> Frame -> Execution Frame
runInstr (Instr op (Num a) (Num b)) symtbl f =
  case op of
    Add -> do
      x <- readRegM a f
      y <- readRegM b f
      f <- writeRegM a (x + y) f
      f <- mapPcM (+ 1) f
      pure f
    Sub -> do
      x <- readRegM a f
      y <- readRegM b f
      f <- writeRegM a (x - y) f
      f <- mapPcM (+ 1) f
      pure f
    Imm -> do
      f <- writeRegM a b f
      f <- mapPcM (+ 1) f
      pure f
    Adi -> do
      x <- readRegM a f
      f <- writeRegM a (x + b) f
      f <- mapPcM (+ 1) f
      pure f
    Sbi -> do
      x <- readRegM a f
      f <- writeRegM a (x - b) f
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
    Ldr -> do
      ma <- readRegM b f
      m <- readMem ma
      f <- writeRegM a m f
      f <- mapPcM (+ 1) f
      pure f
    Str -> do
      ma <- readRegM b f
      x <- readRegM a f
      writeMem ma x
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
    _ -> mapPcM (+ 1) f

runInstr (Instr op (Msg a) (Num b)) symtbl f =
  case op of
    Br -> do
      d <- findsym symtbl a
      f <- setPcM d f
      pure f
    Btr -> do
      x <- readRegM b f
      if x /= 0
        then do
          d <- findsym symtbl a
          setPcM d f
        else mapPcM (+ 1) f
    Bfs -> do
      x <- readRegM b f
      if x == 0
        then do
          d <- findsym symtbl a
          setPcM d f
        else mapPcM (+ 1) f
    _ -> mapPcM (+ 1) f
