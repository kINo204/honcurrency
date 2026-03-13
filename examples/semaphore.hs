import Conc.Semaphore
import Core.Program
import System.Directory.Internal.Prelude (getArgs)

-- An example of using semaphores to manage a pool of resources.
-- There are more workers (threads) than available resources.

numWorkers = 5

numResources = 2

-- The semaphore, representing the pool of resources.
-- Memory addresses are picked arbitrarily.
-- The semaphore's internal value is at address 101.
sem = semaphore numResources 100 200 10

-- Each worker will try to acquire a resource, "work", then release it.
worker = program $ do
  -- Announce intention to work
  tid 0
  prt 0
  prs "enters."

  -- Request a resource from the pool
  semWait sem 0 1

  -- Critical section starts: resource acquired
  tid 0
  prt 0
  prs "resources acquired."

  -- "Work" - just a few instructions to simulate activity
  yld
  yld

  tid 0
  prt 0
  prs "work done!"
  -- Critical section ends

  -- Release the resource back to the pool
  semPost sem 0 1

main :: IO ()
main = do
  args <- getArgs
  let dbg = read $ head args :: Bool
  putStrLn $ "--- Running Semaphore Example (" ++ show numWorkers ++ " workers, " ++ show numResources ++ " resources) ---"
  let progs = replicate numWorkers worker
  let mem = motion sem
  let trace = schedule dbg 10 5 (memory 1000 // mem) progs
  mapM_ putStrLn trace
  putStrLn "--- Simulation Complete ---"