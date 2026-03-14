import Conc.Channel
import Core.Program
import System.Directory.Internal.Prelude (getArgs)

ch1 = channel 0 100 2

ch2 = channel 50 200 2

producer = program $ do
  lab "begin"
  adi 0 1
  send 0 ch1 1 2
  br $ Msg "begin"

doubler = program $ do
  lab "begin"
  recv 0 ch1 1 2
  -- R[0] *= 2:
  imm 1 0
  add 1 0
  add 0 1
  send 0 ch2 1 2
  br $ Msg "begin"

printer = program $ do
  lab "begin"
  recv 0 ch2 1 2
  prt 0
  br $ Msg "begin"

main = do
  args <- getArgs
  let dbg = read $ head args :: Bool
  let mem = memory 1000 // motion ch1 // motion ch2
  let logs = schedule dbg 10 10 mem [producer, doubler, printer]
  mapM_ putStrLn logs

