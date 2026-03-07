import Control.Monad
import Instr
import Machine

type Program = [Instr]

step :: Program -> Frame -> Execution Frame
step prog f =
  let instr = prog !! pc f
   in runInstr instr f

stepN :: Int -> Program -> Frame -> Execution Frame
stepN t prog f
  | t == 1 = step prog f
  | otherwise = do
      f <- step prog f
      stepN (t - 1) prog f

pass :: Int -> [Program] -> [Frame] -> Execution [Frame]
pass t progs frames = sequence [stepN t p f | (p, f) <- zip progs frames]
