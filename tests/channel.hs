import Conc.Channel
import Conc.Cond
import Conc.Mutex
import Control.Monad
import Core.Program
import Utils.Test

-- Test 1: Simple send and receive
-- T0 sends a value, T1 receives it.
sender1 = program $ do
  imm 0 42
  send 0 ch 1 2

receiver1 = program $ do
  recv 0 ch 1 2
  prt 0

-- Test 2: Two senders, one receiver
-- T0 sends 42, T1 sends 43, T2 receives both.
sender2 = program $ do
  imm 0 43
  send 0 ch 1 2

receiverMulti = program $ do
  recv 0 ch 1 2
  prt 0
  recv 1 ch 2 3
  prt 1

-- Shared channel definition for tests
ch = channel 0 100 10 

main :: IO ()
main = do
  let mem = memory 1000

  putStrLn "=== Channel Tests ==="

  runTest
    "Simple send and receive"
    [sender1, receiver1]
    (mem // Conc.Channel.motion ch)
    [Assert "R[0] = 42"]

  runTest
    "Two senders, one receiver"
    [sender1, sender2, receiverMulti]
    (mem // Conc.Channel.motion ch)
    [Assert "R[0] = 42", Assert "R[1] = 43"]
