{- HLINT ignore "Eta reduce" -}
module Utils.Queue
  ( Queue,
    queue,
    motion,
    enqueue,
    dequeue,
    qlength,
  )
where

import Core.Program

data Queue
  = Queue
  { begPointer :: Int,
    endPointer :: Int,
    beg :: Int,
    size :: Int
  }

queue :: Int -> Int -> Int -> Queue
queue addr beg size =
  Queue
    addr
    (addr + 1)
    beg
    size

motion :: Queue -> [(Int, Int)]
motion q =
  [ (begPointer q, beg q),
    (endPointer q, beg q)
  ]

qlength :: Queue -> Int -> Int -> Program
qlength q reg t = procedure $ do
  lod t (begPointer q)
  lod reg (endPointer q)
  sub reg t
  bgez reg $ Num 3
  adi reg (size q)
  br $ Num (-2)

enqueue :: Queue -> Int -> Int -> Program
enqueue q reg t = procedure $ do
  lod t (endPointer q)
  str reg t

  adi t 1
  sbi t (beg q + size q)
  bfs t $ Num 2
  adi t $ size q

  adi t $ beg q
  sto t (endPointer q)

dequeue :: Queue -> Int -> Int -> Program
dequeue q reg t = procedure $ do
  lod t (begPointer q)
  ldr reg t

  adi t 1
  sbi t (beg q + size q)
  bfs t $ Num 2
  adi t $ size q

  adi t $ beg q
  sto t (begPointer q)
