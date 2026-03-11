import Core.Program

-- Test: Simple unconditional branch `br`
-- Jumps over the `imm 1 100` instruction. R1 should remain 0.
unconditionalBranch = program $ do
  imm 1 0
  br (Msg "skip")
  imm 1 100
  lab "skip"
  prt 1

-- Test: `btr` (Branch if True) - Condition met
-- R1 is non-zero, so the branch to "jump" is taken. R2 should remain 0.
branchIfTrueTaken = program $ do
  imm 1 5 -- Condition register
  imm 2 0 -- Test register
  btr 1 (Msg "jump")
  imm 2 100 -- This should be skipped
  lab "jump"
  prt 2

-- Test: `btr` (Branch if True) - Condition NOT met
-- R1 is zero, so the branch is not taken. R2 should be set to 100.
branchIfTrueNotTaken = program $ do
  imm 1 0 -- Condition register
  imm 2 0 -- Test register
  btr 1 (Msg "jump")
  imm 2 100 -- This should be executed
  lab "jump"
  prt 2

-- Test: `bfs` (Branch if False/Zero) - Condition met
-- R1 is zero, so the branch to "jump" is taken. R2 should remain 0.
branchIfFalseTaken = program $ do
  imm 1 0 -- Condition register
  imm 2 0 -- Test register
  bfs 1 (Msg "jump")
  imm 2 100 -- This should be skipped
  lab "jump"
  prt 2

-- Test: `bfs` (Branch if False/Zero) - Condition NOT met
-- R1 is non-zero, so the branch is not taken. R2 should be set to 100.
branchIfFalseNotTaken = program $ do
  imm 1 5 -- Condition register
  imm 2 0 -- Test register
  bfs 1 (Msg "jump")
  imm 2 100 -- This should be executed
  lab "jump"
  prt 2

-- Test: Scoped labels
-- Tests that a jump within a procedure goes to a label inside that procedure's
-- scope, and a jump outside goes to a label in the outer scope.
scopedLabels = program $ do
  imm 1 0 -- Will be set by the procedure
  imm 2 0 -- Will be set by the outer scope
  procedure $ do
    br (Msg "target")
    imm 1 99 -- Should be skipped
    lab "target"
    imm 1 11 -- Should be executed
  br (Msg "target")
  imm 2 99 -- Should be skipped
  lab "target"
  imm 2 22 -- Should be executed
  prt 1
  prt 2

main :: IO ()
main =
  let run p = schedule False 5 (frame 5) (machine 5) [p]
   in do
        putStrLn "--- Test: Simple `br` ---"
        run unconditionalBranch
        putStrLn "\n--- Test: `btr` (branch taken) ---"
        run branchIfTrueTaken
        putStrLn "\n--- Test: `btr` (branch not taken) ---"
        run branchIfTrueNotTaken
        putStrLn "\n--- Test: `bfs` (branch taken) ---"
        run branchIfFalseTaken
        putStrLn "\n--- Test: `bfs` (branch not taken) ---"
        run branchIfFalseNotTaken
        putStrLn "\n--- Test: Scoped labels ---"
        run scopedLabels
