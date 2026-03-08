import Control.Monad
import Instr
import Machine

import Control.Monad.Trans
import Control.Monad.Trans.Writer

type Program = [Instr]

type Trace a = WriterT [String] Execution a

step :: Program -> Frame -> Trace Frame
step prog f =
  let instr = prog !! pc f
   in do
    tell [show instr]
    lift $ runInstr instr f

stepN :: Int -> Program -> Frame -> Trace Frame
stepN t prog f
  | t == 1 = step prog f
  | otherwise = do
      f <- step prog f
      stepN (t - 1) prog f

pass :: Int -> [Program] -> [Frame] -> Trace [Frame]
pass t progs frames =
  sequence
    [ do
        f <- stepN t p f
        tell ["running thread " ++ show i]
        pure f
      | (p, f, i) <- zip3 progs frames [1 .. (length progs)]
    ]
