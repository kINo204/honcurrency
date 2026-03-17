module Core.Program
  ( Program, 
    program, procedure,

    add, sub, imm, adi, sbi,
    lod, sto, ldr, str,
    cas,
    lab, br, btr, bfs, bgz, blz, bgez, blez,
    yld, blk, pst, tid,
    prt, prs,

    Operand(..),
    schedule,
    memory, (//),
  )
where

import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Core.Instr
import Core.Machine
import Core.Scheduler
import Data.Array ((//))

data Scope = Scope Int [Int]

push :: Scope -> Scope
push (Scope next stack) = Scope (next + 1) (next : stack)

pop :: Scope -> Scope
pop (Scope next stack) = Scope next (tail stack)

type Program = StateT Scope (WriterT [Instr] Identity) ()

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
ldr rd rt = emit $ Instr Ldr (Num rd) (Num rt)

str :: Int -> Int -> Program
str rs rt = emit $ Instr Str (Num rs) (Num rt)

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

bgz :: Int -> Operand -> Program
bgz rs dpc@(Num _) = emit $ Instr Bgz dpc (Num rs)
bgz rs (Msg lab) = do
  n <- peek
  emit $ Instr Bgz (Msg $ suffix n lab) (Num rs)

blz :: Int -> Operand -> Program
blz rs dpc@(Num _) = emit $ Instr Blz dpc (Num rs)
blz rs (Msg lab) = do
  n <- peek
  emit $ Instr Blz (Msg $ suffix n lab) (Num rs)

bgez :: Int -> Operand -> Program
bgez rs dpc@(Num _) = emit $ Instr Bgez dpc (Num rs)
bgez rs (Msg lab) = do
  n <- peek
  emit $ Instr Bgez (Msg $ suffix n lab) (Num rs)

blez :: Int -> Operand -> Program
blez rs dpc@(Num _) = emit $ Instr Blez dpc (Num rs)
blez rs (Msg lab) = do
  n <- peek
  emit $ Instr Blez (Msg $ suffix n lab) (Num rs)

yld :: Program
yld = emit $ Instr Yld (Num 0) (Num 0)

tid :: Int -> Program
tid rd = emit $ Instr Tid (Num rd) (Num 0)

blk :: Program
blk = emit $ Instr Blk (Num 0) (Num 0)

pst :: Int -> Program
pst tid = emit $ Instr Pst (Num tid) (Num 0)

prt :: Int -> Program
prt rs = emit $ Instr Prt (Num rs) (Num 0)

prs :: String -> Program
prs msg = emit $ Instr Prs (Msg msg) (Num 0)
