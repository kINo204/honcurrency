module Utils.Test (
  Check (..),
  reportTest,
  runTest,
  ) where

import Control.Monad (unless)
import Core.Instr (Instr)
import Core.Program (schedule)
import Data.List (isInfixOf)

-- Helper to print test result
reportTest :: String -> Bool -> IO ()
reportTest name passed =
  putStrLn $ (if passed then "\x1b[92mPASS\x1b[0m" else "\x1b[91mFAIL\x1b[0m") ++ ": " ++ name

data Check = Assert String | Defute String
type Logs = [String]

check :: [Check] -> Logs -> Bool
check [] _ = True
check ((Assert s) : _) [] = False
check ((Defute s) : _) [] = True
check (c : cs) (l : ls)
  | (Assert s) <- c =
      if s `isInfixOf` l
        then check cs ls -- consume this check and log, and continue checking
        else check (c : cs) ls -- consume this log, and continue checking this check
  | (Defute s) <- c = not (any (s `isInfixOf`) (l : ls)) && check cs (l : ls)

-- Test runners
runTest :: String -> [[Instr]] -> [Check] -> IO ()
runTest name progs checks = do
  let logs = schedule False 10 10 10 progs
  let passed = check checks logs
  reportTest name passed
  unless passed $
    mapM_ putStrLn logs
