module Conc.Spinlock
  ( spinLock,
    spinUnlock,
  )
where

import Core.Instr
import Core.Program

spinLock :: Int -> Int -> Program
spinLock lock t = procedure $ do
  cas t lock
  btr t $ Num (-1)

spinUnlock :: Int -> Int -> Program
spinUnlock lock t = procedure $ do
  imm t 0
  sto t lock
