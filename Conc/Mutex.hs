module Conc.Mutex
  (
    Mutex,
    mutex,
    motion,
    mutexLock,
    mutexUnlock,
  )
where

import Utils.Queue qualified as Q
import Core.Program
import Conc.Spinlock

data Mutex
  = Mutex
  { lock :: Int,
    held :: Int,
    queue :: Q.Queue
  }

mutex :: Int -> Int -> Int -> Mutex
mutex addr qbeg qsize =
  Mutex
    addr
    (addr + 1)
    (Q.queue (addr + 2) qbeg qsize)

motion :: Mutex -> [(Int, Int)]
motion m = Q.motion $ queue m

mutexLock :: Mutex -> Int -> Int -> Program
mutexLock m t0 t1 = procedure $ do
  spinLockYld (lock m) t0

  spinTryLock (held m) t0
  btr t0 $ Msg "mtx_lock_slow"

  spinUnlock (lock m) t0
  br $ Msg "mtx_lock_end"

  lab "mtx_lock_slow"
  tid t0
  Q.enqueue (queue m) t0 t1
  spinUnlock (lock m) t0
  blk

  lab "mtx_lock_end"

mutexUnlock :: Mutex -> Int -> Int -> Program
mutexUnlock m t0 t1 = procedure $ do
  spinLockYld (lock m) t0

  Q.qlength (queue m) t0 t1
  bfs t0 $ Msg "mtx_unlock_fast"

  Q.dequeue (queue m) t0 t1
  spinUnlock (lock m) t1
  pst t0
  br $ Msg "mtx_unlock_end"

  lab "mtx_unlock_fast"
  imm t0 0
  sto t0 (held m)
  spinUnlock (lock m) t0

  lab "mtx_unlock_end"
