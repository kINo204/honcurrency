import Conc.Mutex as M
import Conc.Semaphore as S
import Core.Program
import System.Directory.Internal.Prelude (getArgs)

numObjects = semaphore 0 0 100 10 -- 0 objects at start

numBlanks = semaphore 2 10 110 10 -- 2 buffers left

mtx = mutex 20 120 10

producer i = program $ do
  semWait numBlanks 0 1
  mutexLock mtx 0 1

  prs $ "--> Producer " ++ show i

  mutexUnlock mtx 0 1
  semPost numObjects 0 1

consumer i = program $ do
  semWait numObjects 0 1
  mutexLock mtx 0 1

  prs $ "<-- Consumer " ++ show i

  mutexUnlock mtx 0 1
  semPost numBlanks 0 1

mem =
  memory 200
    // M.motion mtx
    // S.motion numObjects
    // S.motion numBlanks

main = do
  args <- getArgs
  let dbg = read $ head args :: Bool
  let m = 5
  let n = 7
  let producers = [producer i | i <- [0 .. m]]
  let consumers = [consumer i | i <- [0 .. n]]
  let logs = schedule dbg 5 10 mem (producers ++ consumers)
  mapM_ putStrLn logs
