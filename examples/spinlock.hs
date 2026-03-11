import Conc.Spinlock
import Core.Program

p1 = program $ do
  spinLock 0 2
  imm 1 10
  sbi 1 1
  prt 1
  btr 1 $ Num (-2)
  spinUnlock 0 2

p2 = program $ do
  spinLock 0 2
  imm 0 123
  prt 0
  spinUnlock 0 2

main = do
  mapM_ putStrLn (schedule False 5 5 10 [p1, p2])
