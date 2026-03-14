module Conc.Cond
  ( Cond,
    condvar,
    motion,
    condWait,
    condSignal,
  )
where

import Conc.Mutex (Mutex, mutexLock, mutexUnlock)
import Conc.Spinlock (spinLockYld, spinUnlock)
import Core.Program
import Utils.Queue qualified as Q

data Cond
  = Cond
  { lock :: Int,
    queue :: Q.Queue
  }

condvar :: Int -> Int -> Int -> Cond
condvar addr qbeg qsize =
  Cond
    addr
    (Q.queue (addr + 1) qbeg qsize)

motion :: Cond -> [(Int, Int)]
motion = Q.motion . queue

condWait :: Cond -> Mutex -> Int -> Int -> Program
condWait c m t0 t1 = procedure $ do
  spinLockYld (lock c) t0

  tid t0
  Q.enqueue (queue c) t0 t1

  spinUnlock (lock c) t0
  mutexUnlock m t0 t1

  blk

  mutexLock m 0 1

condSignal :: Cond -> Int -> Int -> Program
condSignal c t0 t1 = procedure $ do
  spinLockYld (lock c) t0

  Q.qlength (queue c) t0 t1
  bfs t0 $ Msg "cond_fast"

  Q.dequeue (queue c) t1 t0
  spinUnlock (lock c) t0
  pst t1
  br $ Msg "cond_end"

  lab "cond_fast"
  spinUnlock (lock c) t0

  lab "cond_end"
