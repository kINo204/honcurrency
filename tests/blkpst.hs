import Control.Monad
import Core.Program
import Utils.Test

-- Test 1: Simple block and post
-- Thread 0 blocks, Thread 1 posts to it.
blockAndPost1 = program $ do
  imm 1 1
  blk
  adi 1 1
  prt 1

blockAndPost2 = program $ do
  imm 2 10
  imm 0 0
  pst 0
  prt 2

-- Test 2: Post before block
-- Thread 0 posts to Thread 1 before it blocks.
postBeforeBlock1 = program $ do
  imm 1 1
  pst 1
  imm 1 5
  prt 1

postBeforeBlock2 = program $ do
  imm 2 10
  blk -- Should not block
  adi 2 2
  prt 2

-- Test 3: Multiple blockers
-- Three threads, one posts to only one of the two blocked threads.
multipleBlockers1 = program $ do -- tid 0
  blk
  imm 1 1
  prt 1

multipleBlockers2 = program $ do -- tid 1
  blk
  imm 2 2
  prt 2

multipleBlockers3 = program $ do -- tid 2
  imm 1 1
  pst 1 -- Post to thread 1
  imm 3 3
  prt 3

-- Test 4: Chained unblocking
-- T1 unblocks T0, which then unblocks T2.
chainedUnblocking1 = program $ do -- tid 0
  blk
  imm 1 1
  imm 2 2
  pst 2
  prt 1

chainedUnblocking2 = program $ do -- tid 1
  imm 0 0
  pst 0
  imm 2 2
  prt 2

chainedUnblocking3 = program $ do -- tid 2
  blk
  imm 3 3
  prt 3

main :: IO ()
main = do
  putStrLn "=== Concurrency (Block/Post) Tests ==="
  let mem = memory 10
  runTest
    "Simple block and post"
    [blockAndPost1, blockAndPost2]
    mem
    [Assert "R[2] = 10", Assert "R[1] = 2"]

  runTest
    "Post before block"
    [postBeforeBlock1, postBeforeBlock2]
    mem
    [Assert "R[1] = 5", Assert "R[2] = 12"]

  runTest
    "Multiple blockers, one unblocked"
    [multipleBlockers1, multipleBlockers2, multipleBlockers3]
    mem
    [Defute "R[1] = 1", Assert "R[3] = 3", Assert "R[2] = 2"]

  runTest
    "Chained unblocking"
    [chainedUnblocking1, chainedUnblocking2, chainedUnblocking3]
    mem
    [Assert "R[2] = 2", Assert "R[1] = 1", Assert "R[3] = 3"]
