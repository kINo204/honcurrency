import Core.Program
import Data.List (isInfixOf)
import Control.Monad

-- Helper function to check if a log contains expected output
assert :: String -> [String] -> Bool
assert expected = any (expected `isInfixOf`)

-- Helper to print test result
reportTest :: String -> Bool -> IO ()
reportTest name passed =
  putStrLn $ (if passed then "PASS" else "FAIL") ++ ": " ++ name

-- Test: Basic yield execution
-- Yield instruction should execute and move program counter to the next instruction.
-- R1 should be set to 42.
basicYield = program $ do
  imm 1 42
  yld
  prt 1

-- Test: Yield doesn't affect single-threaded behavior
-- The program should execute normally, with R1 = 99 and R2 = 11.
yieldSingleThread = program $ do
  imm 1 99
  yld
  imm 2 11
  yld
  prt 1
  prt 2

-- Test: Yield allows interleaving in multi-threaded context
-- Thread 1: Yields after setting R1, then increments it
-- Thread 2: Increments R2 multiple times before yielding
-- With timestep=2, we expect interleaving due to yields.
-- Thread1: imm 1 10 (step1), yld (step2, yields)
-- Thread2: imm 2 0, add 2 2, add 2 2 (step1-3)
yieldMultithread = program $ do
  imm 1 10
  yld
  adi 1 1
  prt 1

thread2 = program $ do
  imm 1 0
  adi 1 2
  adi 1 2
  adi 1 2
  prt 1

-- Test: Yield prevents thread starvation
-- Two threads with small timesteps should both make progress.
-- Thread 1: Adds 5 to R1, yields, repeats
-- Thread 2: Waiting to increment a counter in shared memory
yieldFairness = program $ do
  imm 1 0
  adi 1 5
  yld
  adi 1 5
  yld
  adi 1 5
  prt 1

-- Test: Multiple yields in sequence
-- Demonstrates that consecutive yields work correctly.
-- R1 should be 77.
multipleYields = program $ do
  imm 1 77
  yld
  yld
  yld
  prt 1

-- Test: Yield in a loop
-- Sets R1 to 0, then increments 3 times with yields between.
-- R1 should be 3.
yieldInLoop = program $ do
  imm 1 0
  adi 1 1
  yld
  adi 1 1
  yld
  adi 1 1
  yld
  prt 1

main :: IO ()
main =
  do
    let runSingle p = schedule True 10 (frame 10) (machine 10) [p]
        runDual p1 p2 = schedule True 2 (frame 10) (machine 10) [p1, p2]
        runTest name prog expected = do
          let logs = runSingle prog
          let passed = all (`assert` logs) expected
          reportTest name passed
          unless passed $ mapM_ putStrLn logs
        runTestDual name p1 p2 expected = do
          let logs = runDual p1 p2
          let passed = all (`assert` logs) expected
          reportTest name passed
          unless passed $ mapM_ putStrLn logs
    putStrLn "=== Yield Tests ==="
    runTest "Basic yield (set R1 to 42)" basicYield ["R[1] = 42"]
    runTest "Yield in single-threaded (R1=99, R2=11)" yieldSingleThread ["R[1] = 99", "R[2] = 11"]
    runTest "Multiple yields in sequence (R1=77)" multipleYields ["R[1] = 77"]
    runTest "Yield in loop (R1=3)" yieldInLoop ["R[1] = 3"]
    runTest "Yield fairness (R1=15)" yieldFairness ["R[1] = 15"]
    runTestDual "Multi-threaded yield (R1=11, R1=6)" yieldMultithread thread2 ["R[1] = 11", "R[1] = 6"]
