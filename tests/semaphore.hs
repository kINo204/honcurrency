import Conc.Semaphore
import Control.Monad
import Core.Program
import Utils.Test

-- Test 1: Simple wait and post
-- T0 waits on a semaphore with value 0, T1 posts to it. T0 should unblock.
semWaitPost1 = program $ do
  semWait s 0 1
  imm 1 1
  prt 1

semWaitPost2 = program $ do
  semPost s 0 1

-- Test 2: Post before wait
-- T0 posts to a semaphore with value 0, making it 1. T1 waits and should not block.
semPostWait1 = program $ do
  semPost s 0 1
  imm 1 1
  prt 1

semPostWait2 = program $ do
  semWait s 0 1
  imm 2 2
  prt 2

-- Test 3: Mutual exclusion (N=1)
-- Two threads increment a shared counter. With a mutex (sem N=1), final value should be 2.
counterAddr = 80

mutexWorker1 = program $ do
  semWait s_n1 0 1
  lod 1 counterAddr
  adi 1 1
  sto 1 counterAddr
  semPost s_n1 0 1

mutexWorker2 = program $ do
  semWait s_n1 0 1
  lod 2 counterAddr
  adi 2 1
  sto 2 counterAddr
  semPost s_n1 0 1
  lod 3 counterAddr
  prt 3

-- Test 4: N > 1 resources
-- Semaphore initialized to 2. Three threads try to acquire. Two should succeed, one should block.
nResWorker1 = program $ do semWait s_n2 0 1; imm 1 11; prt 1; blk

nResWorker2 = program $ do semWait s_n2 0 1; imm 2 22; prt 2; blk

nResWorker3 = program $ do semWait s_n2 0 1; imm 3 33; prt 3; blk

-- Shared semaphore definitions for tests
s = semaphore 100 200 10

s_n1 = semaphore 110 210 10

s_n2 = semaphore 120 220 10

main :: IO ()
main = do
  let mem = memory 1000

  putStrLn "=== Semaphore Tests ==="

  runTest
    "Simple wait and post"
    [semWaitPost1, semWaitPost2]
    (mem // (motion s ++ [(101, 0)]))
    [Assert "R[1] = 1"]

  runTest
    "Post before wait"
    [semPostWait1, semPostWait2]
    (mem // (motion s ++ [(101, 0)]))
    [Assert "R[1] = 1", Assert "R[2] = 2"]

  runTest
    "Mutual exclusion (N=1)"
    [mutexWorker1, mutexWorker2]
    (mem // (motion s_n1 ++ [(111, 1), (counterAddr, 0)]))
    [Assert "R[3] = 2"]

  runTest
    "N resources (N=2), 3 workers"
    [nResWorker1, nResWorker2, nResWorker3]
    (mem // (motion s_n2 ++ [(121, 2)]))
    [Assert "R[1] = 11", Assert "R[2] = 22", Defute "R[3] = 33"]