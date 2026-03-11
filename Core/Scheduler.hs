module Core.Scheduler (schedule) where

import Prelude as P
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer hiding (pass)
import Core.Instr
import Core.Machine
import Data.Map qualified as Map
import Data.List (zip4)

type Program = [Instr]

type Trace a = WriterT [String] Execution a

traceOf :: Trace a -> Execution [String]
traceOf = execWriterT

step :: Bool -> Program -> SymTbl -> Frame -> Trace Frame
step dbg prog symtbl f
  | done = pure f
  | otherwise =
      let instr = prog !! pc f
       in do
            when dbg $ tell [show (pc f) ++ ": " ++ show instr]
            case instr of
              (Instr Prt (Num rs) _) -> do
                x <- lift $ readRegM rs f
                tell ["R[" ++ show rs ++ "] = " ++ show x]
                lift $ mapPcM (+ 1) f
              (Instr Prs (Msg msg) _) -> do
                tell [msg]
                lift $ mapPcM (+ 1) f
              _ -> lift $ runInstr instr symtbl f
  where
    done = pc f >= length prog

stepN :: Bool -> Int -> Program -> SymTbl -> Frame -> Trace Frame
stepN dbg t prog symtbl f
  | t == 1 = step dbg prog symtbl f
  | otherwise = do
      f <- step dbg prog symtbl f
      stepN dbg (t - 1) prog symtbl f

once :: Bool -> Int -> [Program] -> [SymTbl] -> [Frame] -> Trace [Frame]
once dbg t progs symtbls frames =
  sequence
    [ let done = pc f >= length p
       in do
            when (dbg && not done) $
              tell ["running thread " ++ show i]
            stepN dbg t p s f
      | (p, f, s, i) <- zip4 progs frames symtbls [1 .. (length progs)]
    ]

dones :: [Program] -> [Frame] -> Trace [Bool]
dones progs frames =
  sequence
    [ let done = pc f >= length p
       in pure done
      | (p, f) <- zip progs frames
    ]

loop :: Bool -> Int -> [Program] -> [SymTbl] -> [Frame] -> Trace [Frame]
loop dbg t progs symtbls frames = do
  ds <- dones progs frames
  if not $ and ds
    then do
      frames <- once dbg t progs symtbls frames
      loop dbg t progs symtbls frames
    else pure frames

symtbl :: Program -> SymTbl
symtbl instrs = symtbl' instrs 0
  where
    symtbl' :: Program -> Int -> SymTbl
    symtbl' instrs lineNo
      | P.null instrs = Map.empty
      | otherwise =
          let i : is = instrs
              rest = symtbl' is (lineNo + 1)
           in case i of
                Instr Lab (Msg l) _ -> Map.insert l lineNo rest
                _ -> rest

schedule :: Bool -> Int -> Frame -> Machine -> [Program] -> IO ()
schedule dbg timesteps frame machine programs =
  let n = length programs
      symtbls = P.map symtbl programs
      execution = loop dbg timesteps programs symtbls (replicate n frame)
      (machine', logs) = execute (traceOf execution) machine
   in do
        forM_ logs putStrLn
