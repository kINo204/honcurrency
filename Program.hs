module Program
  ( program,
    add,
    sub,
    imm,
    lod,
    sto,
    cas,
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
add a b = emit $ Instr Add a b

sub :: Int -> Int -> Program
sub a b = emit $ Instr Sub a b

imm :: Int -> Int -> Program
imm a b = emit $ Instr Imm a b

lod :: Int -> Int -> Program
lod a b = emit $ Instr Lod a b

sto :: Int -> Int -> Program
sto a b = emit $ Instr Sto a b

cas :: Int -> Int -> Program
cas a b = emit $ Instr Cas a b
