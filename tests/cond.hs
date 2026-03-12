import Conc.Cond as C
import Conc.Mutex as M
import Core.Program
import Utils.Test

-- Test 1: Simple wait and signal, properly synchronized
-- T0 waits, then T1 signals it. T0 should unblock.
condWaitSignal1 = program $ do
  mutexLock m 0 1
  prs "P1 entering CS."
  condWait c m 0 1
  prs "P1 leaving CS."
  mutexUnlock m 0 1

condWaitSignal2 = program $ do
  mutexLock m 0 1
  prs "P2 entering CS."
  condSignal c 0 1 -- Signal T0
  mutexUnlock m 0 1

-- Test 2: Signal with no waiters
-- T0 signals before T1 waits. T1 should block, as the signal is missed.
signalNoWaiter1 = program $ do
  condSignal c_t2 0 1 -- Should be a no-op
  imm 1 1
  pst 1 -- Start T1
  imm 1 11
  prt 1

signalNoWaiter2 = program $ do
  blk -- Wait for T0 to signal
  mutexLock m_t2 0 1
  condWait c_t2 m_t2 0 1 -- Should block
  imm 2 22
  prt 2
  mutexUnlock m_t2 0 1

-- Test 3: condWait releases the mutex
-- T0 locks and waits. T1 should be able to acquire the same lock.
waitReleasesMutex1 = program $ do
  mutexLock m_t3 0 1
  imm 1 1
  pst 1 -- Start T1
  condWait c_t3 m_t3 0 1
  mutexUnlock m_t3 0 1
  imm 1 111
  prt 1

waitReleasesMutex2 = program $ do
  blk -- Wait for T0
  mutexLock m_t3 0 1 -- Should succeed because T0 is in condWait
  imm 2 222
  prt 2
  condSignal c_t3 0 1 -- Signal T0 to let it finish
  mutexUnlock m_t3 0 1

-- Shared resources for tests
m = mutex 100 200 10

c = condvar 110 300 10

m_t2 = mutex 120 210 10

c_t2 = condvar 130 310 10

m_t3 = mutex 140 220 10

c_t3 = condvar 150 320 10

main :: IO ()
main = do
  putStrLn "=== Condition Variable Tests ==="

  let mem = memory 1000

  runTest
    "Simple wait and signal"
    [condWaitSignal1, condWaitSignal2]
    (mem // (M.motion m ++ C.motion c))
    [Assert "P1 entering CS.", Assert "P2 entering CS.", Assert "P1 leaving CS."]

  runTest
    "Signal with no waiters (should miss)"
    [signalNoWaiter1, signalNoWaiter2]
    (mem // (M.motion m_t2 ++ C.motion c_t2))
    [Assert "R[1] = 11", Defute "R[2] = 22"]

  runTest
    "condWait releases mutex"
    [waitReleasesMutex1, waitReleasesMutex2]
    (mem // (M.motion m_t3 ++ C.motion c_t3))
    [Assert "R[2] = 222", Assert "R[1] = 111"]