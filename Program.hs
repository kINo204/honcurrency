module Program
  ( program,
    add,
    sub,
    imm,
    lod,
    sto,
  )
where

import Control.Monad.Writer
import Instr

type Program = Writer [Instr] ()

program :: Program -> [Instr]
program = execWriter

emit :: Instr -> Program
emit i = tell [i]

add :: Operand -> Operand -> Program
add a b = emit $ Instr Add a b

sub :: Operand -> Operand -> Program
sub a b = emit $ Instr Sub a b

imm :: Operand -> Operand -> Program
imm a b = emit $ Instr Imm a b

lod :: Operand -> Operand -> Program
lod a b = emit $ Instr Lod a b

sto :: Operand -> Operand -> Program
sto a b = emit $ Instr Sto a b
