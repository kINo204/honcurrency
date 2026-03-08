module Program
  ( program,
    add, sub, imm,
    lod, sto,
    cas,
    br, btr, bfs,
  )
where

import Control.Monad.Writer
import Instr

type Program = Writer [Instr] ()

program :: Program -> [Instr]
program = execWriter

emit :: Instr -> Program
emit i = tell [i]

add :: Int -> Int -> Program
add rd rs = emit $ Instr Add (Num rd) (Num rs)

sub :: Int -> Int -> Program
sub rd rs = emit $ Instr Sub (Num rd) (Num rs)

imm :: Int -> Int -> Program
imm rd i = emit $ Instr Imm (Num rd) (Num i)

lod :: Int -> Int -> Program
lod rd ma = emit $ Instr Lod (Num rd) (Num ma)

sto :: Int -> Int -> Program
sto rs ma = emit $ Instr Sto (Num rs) (Num ma)

cas :: Int -> Int -> Program
cas rd ma = emit $ Instr Cas (Num rd) (Num ma)

br  :: Operand -> Program
br dpc@(Num _) = emit $ Instr Br dpc (Num 0)
br lab@(Msg _) = emit $ Instr Br lab (Num 0)

btr :: Int -> Operand -> Program
btr rs dpc@(Num _) = emit $ Instr Btr dpc (Num rs)
btr rs lab@(Msg _) = emit $ Instr Btr lab (Num rs)

bfs :: Int -> Operand -> Program
bfs rs dpc@(Num _) = emit $ Instr Bfs dpc (Num rs)
bfs rs lab@(Msg _) = emit $ Instr Bfs lab (Num rs)
