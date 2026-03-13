import Conc.Mutex
import Core.Program
import System.Directory.Internal.Prelude (getArgs)

mtx = mutex 0 10 10

p1 = program $ do
  prs "P1: enters."

  mutexLock mtx 2 3
  imm 1 2
  sbi 1 1
  prs "P1: occupying the lock ..."
  btr 1 $ Num (-2)
  mutexUnlock mtx 2 3

p2 = program $ do
  prs "P2: enters."
  mutexLock mtx 0 1
  prs "P2: I acquire the lock again!"
  mutexUnlock mtx 0 1

mem =
  memory 50
    // motion mtx

main = do
  args <- getArgs
  let dbg = read $ head args :: Bool
  let logs = schedule dbg 10 10 mem [p1, p2]
  mapM_ putStrLn logs
