module Conc.Semaphore
  ( Semaphore,
    semaphore,
    motion,
    semWait,
    semPost,
  )
where

import Conc.Spinlock
import Core.Program
import Utils.Queue qualified as Q

data Semaphore
  = Semaphore
  { lock :: Int,
    value :: Int,
    queue :: Q.Queue
  }

semaphore :: Int -> Int -> Int -> Semaphore
semaphore addr qbeg qsize =
  Semaphore
    addr
    (addr + 1)
    (Q.queue (addr + 2) qbeg qsize)

motion :: Semaphore -> [(Int, Int)]
motion = Q.motion . queue

semWait :: Semaphore -> Int -> Int -> Program
semWait s t0 t1 = procedure $ do
  spinLockYld (lock s) t0

  lod t0 (value s)
  bfs t0 $ Msg "sem_wait_slow"

  sbi t0 1
  sto t0 (value s)
  spinUnlock (lock s) t0
  br $ Msg "sem_wait_end"

  lab "sem_wait_slow"
  tid t0
  Q.enqueue (queue s) t0 t1
  spinUnlock (lock s) t0
  blk

  lab "sem_wait_end"

semPost :: Semaphore -> Int -> Int -> Program
semPost s t0 t1 = procedure $ do
  spinLockYld (lock s) t0

  Q.qlength (queue s) t0 t1
  bfs t0 $ Msg "sem_post_empty"

  Q.dequeue (queue s) t0 t1
  spinUnlock (lock s) t1
  pst t0
  br $ Msg "sem_post_end"

  lab "sem_post_empty"
  lod t0 (value s)
  adi t0 1
  sto t0 (value s)
  spinUnlock (lock s) t0

  lab "sem_post_end"
