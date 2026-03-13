import Conc.Spinlock
import Core.Program
import System.Directory.Internal.Prelude (getArgs)

p1 = program $ do
  spinLockYld 0 2
  prs "TID = 0 in Critical Section!"
  prs "TID = 0 working ..."
  prs "TID = 0 working ..."
  prs "TID = 0 working ..."
  spinUnlock 0 2

p2 = program $ do
  spinLockYld 0 2
  prs "TID = 1 in Critical Section!"
  spinUnlock 0 2

main = do
  args <- getArgs
  let dbg = read $ head args :: Bool
  mapM_ putStrLn $ schedule dbg 5 10 (memory 10) [p1, p2]
