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

step :: Program -> Frame -> Trace Frame
step prog f
  | done = pure f
  | otherwise =
      let instr = prog !! pc f
       in do
            tell [show (pc f) ++ ": " ++ show instr]
            case instr of
              (Instr Prt (Num rs) _) -> do
                x <- lift $ readRegM rs f
                tell ["R[" ++ show rs ++ "] = " ++ show x]
                pure f
              (Instr Prs (Msg msg) _) -> do
                tell [msg]
                pure f
              _ -> lift $ runInstr instr f
  where
    done = pc f >= length prog

stepN :: Int -> Program -> Frame -> Trace Frame
stepN t prog f
  | t == 1 = step prog f
  | otherwise = do
      f <- step prog f
      stepN (t - 1) prog f

once :: Int -> [Program] -> [Frame] -> Trace [Frame]
once t progs frames =
  sequence
    [ do
        tell ["running thread " ++ show i]
        stepN t p f
      | (p, f, i) <- zip3 progs frames [1 .. (length progs)]
    ]

dones :: [Program] -> [Frame] -> Trace [Bool]
dones progs frames =
  sequence
    [ let done = pc f >= length p
       in pure done
      | (p, f) <- zip progs frames
    ]

loop :: Int -> [Program] -> [Frame] -> Trace [Frame]
loop t progs frames = do
  ds <- dones progs frames
  if not $ and ds
    then do
      frames <- once t progs frames
      loop t progs frames
    else pure frames

schedule :: Int -> [Program] -> Frame -> Machine -> IO ()
schedule timesteps programs frame machine =
  let n = length programs
      execution = loop timesteps programs (replicate n frame) -- TODO: not once!
      (machine', logs) = execute (traceOf execution) machine
   in do
    print machine'
    forM_ logs putStrLn
