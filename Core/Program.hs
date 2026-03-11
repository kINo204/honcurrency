module Core.Program
  ( Program, 
    program, procedure,

    add, sub, imm, adi, sbi,
    lod, sto,
    cas,
    lab, br, btr, bfs, yld,
    prt, prs,

    frame, machine,
    Operand(..),
    schedule,
  )
where

import Control.Monad.State
import Control.Monad.Writer
import Core.Instr
import Core.Machine
import Core.Scheduler

data Scope = Scope Int [Int]

push :: Scope -> Scope
push (Scope next stack) = Scope (next + 1) (next : stack)

pop :: Scope -> Scope
pop (Scope next stack) = Scope next (tail stack)

type Program = StateT Scope (Writer [Instr]) ()

program :: Program -> [Instr]
program p = execWriter $ runStateT p (Scope 1 [0])

emit :: Instr -> Program
emit i = tell [i]

procedure :: Program -> Program
procedure program = do
  scope <- get
  put $ push scope
  program
  scope <- get
  put $ pop scope

peek :: StateT Scope (Writer [Instr]) Int
peek = do
  Scope _ stack <- get
  return $ head stack

add :: Int -> Int -> Program
add rd rs = emit $ Instr Add (Num rd) (Num rs)

sub :: Int -> Int -> Program
sub rd rs = emit $ Instr Sub (Num rd) (Num rs)

imm :: Int -> Int -> Program
imm rd i = emit $ Instr Imm (Num rd) (Num i)

adi :: Int -> Int -> Program
adi rd im = emit $ Instr Adi (Num rd) (Num im)

sbi :: Int -> Int -> Program
sbi rd im = emit $ Instr Sbi (Num rd) (Num im)

lod :: Int -> Int -> Program
lod rd ma = emit $ Instr Lod (Num rd) (Num ma)

sto :: Int -> Int -> Program
sto rs ma = emit $ Instr Sto (Num rs) (Num ma)

ldr :: Int -> Int -> Program
ldr rd rt = emit $ Instr Lod (Num rd) (Num rt)

str :: Int -> Int -> Program
str rs rt = emit $ Instr Sto (Num rs) (Num rt)

cas :: Int -> Int -> Program
cas rd ma = emit $ Instr Cas (Num rd) (Num ma)

suffix :: Int -> String -> String
suffix scopeNo str = str ++ "@" ++ show scopeNo

lab :: String -> Program
lab label = do
  n <- peek
  emit $ Instr Lab (Msg $ suffix n label) (Num 0)

br :: Operand -> Program
br dpc@(Num _) = emit $ Instr Br dpc (Num 0)
br (Msg lab) = do
  n <- peek
  emit $ Instr Br (Msg $ suffix n lab) (Num 0)

btr :: Int -> Operand -> Program
btr rs dpc@(Num _) = emit $ Instr Btr dpc (Num rs)
btr rs (Msg lab) = do
  n <- peek
  emit $ Instr Btr (Msg $ suffix n lab) (Num rs)

bfs :: Int -> Operand -> Program
bfs rs dpc@(Num _) = emit $ Instr Bfs dpc (Num rs)
bfs rs (Msg lab) = do
  n <- peek
  emit $ Instr Bfs (Msg $ suffix n lab) (Num rs)

yld :: Program
yld = emit $ Instr Yld (Num 0) (Num 0)

prt :: Int -> Program
prt rs = emit $ Instr Prt (Num rs) (Num 0)

prs :: String -> Program
prs msg = emit $ Instr Prs (Msg msg) (Num 0)
