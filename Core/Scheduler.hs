module Core.Scheduler (schedule) where

import Control.Monad
import Control.Monad.Writer hiding (pass)
import Core.Instr
import Core.Machine
import Data.Map qualified as Map
import Data.List (zip4)

type Program = [Instr]

type Trace a = WriterT [String] Execution a

traceOf :: Trace a -> Execution [String]
traceOf = execWriterT

step :: Bool -> Program -> Int -> Frame -> Trace Frame
step dbg prog tid f = do
  blocked <- lift $ isBlocked tid
  if blocked
    then pure f
    else do
      let instr = prog !! pc f
      when dbg $
        tell
          [ "TID " ++ show tid ++ " " ++
            "[PC=" ++ show (pc f) ++ "]: " ++
            show instr
          ]
      case instr of
        (Instr Prt (Num rs) _) -> do
          x <- lift $ readRegM rs f
          tell ["R[" ++ show rs ++ "] = " ++ show x]
          lift $ mapPcM (+ 1) f
        (Instr Prs (Msg msg) _) -> do
          tell [msg]
          lift $ mapPcM (+ 1) f
        _ -> lift $ runInstr instr tid f

stepN :: Bool -> Int -> Program -> Int -> Frame -> Trace Frame
stepN dbg t prog tid f
  | pc f >= length prog = pure f
  | t == 1 = step dbg prog tid f
  | (Instr Yld _ _) <- prog !! pc f = step dbg prog tid f
  | otherwise = do
      f <- step dbg prog tid f
      stepN dbg (t - 1) prog tid f

once :: Bool -> Int -> [Program] -> [Frame] -> Trace [Frame]
once dbg t progs frames =
  sequence
    [ let finished = pc f >= length p
       in do
        blocked <- lift $ isBlocked tid
        let done = finished || blocked
        when (dbg && not done) $
          tell ["\x1B[34mScheduling TID = " ++ show tid ++ "\x1B[0m"]
        stepN dbg t p tid f
      | (p, f, tid) <- zip3 progs frames [0 .. length progs - 1]
    ]

dones :: [Program] -> [Frame] -> Trace [Bool]
dones progs frames =
  sequence
    [ let finished = pc f >= length p
       in do
        blocked <- lift $ isBlocked tid
        let done = finished || blocked
        pure done
      | (p, f, tid) <- zip3 progs frames [0 .. length progs - 1]
    ]

loop :: Bool -> Int -> [Program] -> [Frame] -> Trace [Frame]
loop dbg t progs frames = do
  ds <- dones progs frames
  if not $ and ds
    then do
      frames <- once dbg t progs frames
      loop dbg t progs frames
    else pure frames

makeSymTbl :: Program -> SymTbl
makeSymTbl instrs = symtbl' instrs 0
  where
    symtbl' :: Program -> Int -> SymTbl
    symtbl' instrs lineno
      | null instrs = Map.empty
      | otherwise =
          let i : is = instrs
              rest = symtbl' is (lineno + 1)
           in case i of
                Instr Lab (Msg l) _ -> Map.insert l lineno rest
                _ -> rest

schedule :: Bool -> Int -> Int -> Mem -> [Program] -> [String]
schedule dbg timesteps nregs mem programs =
  let n = length programs
      frames = replicate n $ frame nregs
      symtbls = map makeSymTbl programs
      m = machine n mem symtbls
      execution = loop dbg timesteps programs frames
      (machine', logs) = execute (traceOf execution) m
   in logs
