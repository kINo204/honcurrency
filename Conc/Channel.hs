module Conc.Channel
  ( Channel,
    channel,
    Conc.Channel.motion,
    send,
    recv,
  )
where

import Conc.Cond as C
import Conc.Mutex as M
import Core.Program
import Utils.Queue as Q

data Channel
  = Channel
  { buf :: Int, -- 1
    lock :: Mutex, -- 4
    senders :: Queue, -- 2
    recvers :: Queue, -- 2
    ready :: Cond -- 3
  }

channel :: Int -> Int -> Int -> Channel
channel addr qbeg qsize =
  Channel
    addr
    (mutex (addr + 1) qbeg qsize)
    (queue (addr + 5) (qbeg + qsize) qsize)
    (queue (addr + 7) (qbeg + 2 * qsize) qsize)
    (condvar (addr + 9) (qbeg + 3 * qsize) qsize)

motion :: Channel -> [(Int, Int)]
motion chan =
  concat
    [ M.motion $ lock chan,
      Q.motion $ senders chan,
      Q.motion $ recvers chan,
      C.motion $ ready chan
    ]

send :: Int -> Channel -> Int -> Int -> Program
send rs chan t0 t1 = procedure $ do
  mutexLock (lock chan) t0 t1
  qlength (recvers chan) t0 t1 -- no receivers waiting, slow
  bfs t0 $ Msg "channel_send_slow"

  sto rs (buf chan) -- transfer data
  dequeue (recvers chan) t0 t1 -- fetch the first receiver
  pst t0
  br $ Msg "channel_send_end"

  lab "channel_send_slow"
  tid t0
  enqueue (senders chan) t0 t1
  mutexUnlock (lock chan) t0 t1
  blk
  mutexLock (lock chan) t0 t1
  sto rs (buf chan) -- transfer data
  condSignal (ready chan) t0 t1

  lab "channel_send_end"
  mutexUnlock (lock chan) t0 t1

recv :: Int -> Channel -> Int -> Int -> Program
recv rd chan t0 t1 = procedure $ do
  mutexLock (lock chan) t0 t1
  qlength (senders chan) t0 t1 -- no senders waiting, slow
  bfs t0 $ Msg "channel_recv_slow"

  dequeue (senders chan) t0 t1 -- fetch the first sender
  pst t0
  condWait (ready chan) (lock chan) t0 t1
  lod rd (buf chan)
  br $ Msg "channel_recv_end"

  lab "channel_recv_slow"
  tid t0
  enqueue (recvers chan) t0 t1
  mutexUnlock (lock chan) t0 t1
  blk
  mutexLock (lock chan) t0 t1
  lod rd (buf chan)

  lab "channel_recv_end"
  mutexUnlock (lock chan) t0 t1
