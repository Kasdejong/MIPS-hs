{-# LANGUAGE DataKinds #-}
import Data.Bits (Bits(shiftR))
import Data.Int

import Util
import Operations
import Alu
main = print "hi"


data WIRES = Wires {cond :: Bool, alu_o :: Int}
data MEMORY = Memory {mem, regs :: [Int], wires :: WIRES, pc :: Int}

data IF_ID = IfId {if_ir, if_pc :: Int} deriving Show
data ID_EX = IdEx {id_ir, id_a, id_b, id_pc, id_imm :: Int} deriving Show
data EX_MEM = ExMem {ex_ir, ex_alu_o, ex_b :: Int} deriving Show
data MEM_WB = MemWb {mem_ir, mem_alu_o, mem_lmd :: Int} deriving Show

data PlReg where
  IF_ID_REG :: IF_ID -> PlReg
  ID_EX_REG :: ID_EX -> PlReg
  EX_MEM_REG :: EX_MEM -> PlReg
  MEM_WB_REG :: MEM_WB -> PlReg
  deriving Show

class Tickable state input output where
    tick :: state -> input -> (state, output)


-- IF stage
instance Tickable MEMORY (Maybe reg) IF_ID where
    tick :: MEMORY -> Maybe reg -> (MEMORY, IF_ID)
    tick state Nothing = 
        (state, IfId{
            if_ir = mem state !! pc state, 
            if_pc = if cond (wires state) then alu_o (wires state) else pc state + 4
        })

-- ID stage
instance Tickable MEMORY IF_ID ID_EX where
    tick :: MEMORY -> IF_ID -> (MEMORY, ID_EX)
    tick state input = 
        (state, IdEx {
            id_a = regs state !! rs (if_ir input), 
            id_b = regs state !! rt (if_ir input), 
            id_pc = if_pc input, 
            id_ir = if_ir input, 
            id_imm = imm (if_ir input)
        })

instance Tickable MEMORY ID_EX EX_MEM where
    tick :: MEMORY -> ID_EX -> (MEMORY, EX_MEM)
    tick state input = 
        case instrCat code of 
            Alu -> 
                (state,ExMem{
                    ex_ir = id_ir input, 
                    ex_alu_o = case encodingType code of
                        RType -> compute code (id_a input) (id_b input)
                        IType -> compute code (id_a input) (id_imm input),
                    ex_b = 0
                })   
            LS -> 
                (state, ExMem{
                    ex_ir = id_ir input,
                    ex_alu_o = id_a input + id_imm input,
                    ex_b = id_b input
                })
            Br -> 
                (
                    Memory{
                        mem = mem state,
                        regs = regs state,
                        wires = 
                            Wires{
                                cond = id_a input == 0,
                                alu_o = id_pc input + id_imm input * 4
                            },
                        pc = pc state
                    },
                    ExMem{
                        ex_alu_o = 0, 
                        ex_b = 0, 
                        ex_ir = 0
                    } -- does not matter, our branch instruction is complete. ir 0 is a no op
                )
        where code = opcode (id_ir input)

instance Tickable MEMORY EX_MEM MEM_WB where
    tick :: MEMORY -> EX_MEM -> (MEMORY, MEM_WB)
    tick state input | ex_ir input == 0 = (state, MemWb{mem_alu_o = 0, mem_ir = 0, mem_lmd = 0})
    tick state input = 
        case instrCat code of 
            Alu -> 
                (state, MemWb {
                    mem_ir = ex_ir input,
                    mem_alu_o = ex_alu_o input,
                    mem_lmd = 0
                })
            LS -> handleLS state input
        where code = opcode (ex_ir input)



handleLS :: MEMORY -> EX_MEM -> (MEMORY, MEM_WB)
handleLS a b = (Memory{}, MemWb{})