module Conc.Spinlock
  ( spinLock,
    spinUnlock,
    spinTryLock,
    spinLockYld
  )
where

import Core.Instr
import Core.Program

spinLock lock t = procedure $ do
  cas t lock
  btr t $ Num (-1)

spinTryLock lock t = procedure $ do
  cas t lock

spinUnlock lock t = procedure $ do
  imm t 0
  sto t lock

spinLockYld lock t = procedure $ do
  cas t lock
  bfs t $ Num 3
  yld
  br $ Num (-3)
