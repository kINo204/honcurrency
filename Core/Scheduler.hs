module Core.Scheduler (schedule) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer hiding (pass)
import Core.Instr
import Core.Machine

type Program = [Instr]

type Trace a = WriterT [String] Execution a

traceOf :: Trace a -> Execution [String]
traceOf = execWriterT

step :: Bool -> Program -> Frame -> Trace Frame
step dbg prog f
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
              _ -> lift $ runInstr instr f
  where
    done = pc f >= length prog

stepN :: Bool -> Int -> Program -> Frame -> Trace Frame
stepN dbg t prog f
  | t == 1 = step dbg prog f
  | otherwise = do
      f <- step dbg prog f
      stepN dbg (t - 1) prog f

once :: Bool -> Int -> [Program] -> [Frame] -> Trace [Frame]
once dbg t progs frames =
  sequence
    [ let done = pc f >= length p
       in do
            when (dbg && not done) $
              tell ["running thread " ++ show i]
            stepN dbg t p f
      | (p, f, i) <- zip3 progs frames [1 .. (length progs)]
    ]

dones :: [Program] -> [Frame] -> Trace [Bool]
dones progs frames =
  sequence
    [ let done = pc f >= length p
       in pure done
      | (p, f) <- zip progs frames
    ]

loop :: Bool -> Int -> [Program] -> [Frame] -> Trace [Frame]
loop dbg t progs frames = do
  ds <- dones progs frames
  if not $ and ds
    then do
      frames <- once dbg t progs frames
      loop dbg t progs frames
    else pure frames

schedule :: Bool -> Int -> [Program] -> Frame -> Machine -> IO ()
schedule dbg timesteps programs frame machine =
  let n = length programs
      execution = loop dbg timesteps programs (replicate n frame) -- TODO: not once!
      (machine', logs) = execute (traceOf execution) machine
   in do
    forM_ logs putStrLn
