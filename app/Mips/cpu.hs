{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Mips.CPU where
import Data.Bits (Bits(shiftR))
import Data.Int
import Data.Binary
import Prelude hiding ((!!))

import Mips.Util
import Mips.Operations
import Mips.Alu
import Mips.Memory

data IF_ID = IfId {if_ir :: Word32, if_pc :: Int} deriving Show
data ID_EX = IdEx {id_ir, id_a, id_b :: Word32, id_pc :: Int, id_imm :: Word16} deriving Show
data EX_MEM = ExMem {ex_ir, ex_alu_o, ex_b :: Word32} deriving Show
data MEM_WB = MemWb {mem_ir, mem_alu_o, mem_lmd :: Word32} deriving Show

class Tickable state input output where
    tick :: state -> input -> (state, output)


-- IF stage
instance Tickable MEMORY (Maybe reg) IF_ID where
    tick :: MEMORY -> Maybe reg -> (MEMORY, IF_ID)
    tick state Nothing = 
        (state, IfId{
            if_ir = readMemWord (pc state) (mem state), 
            if_pc = if cond (wires state) then fromIntegral (alu_o (wires state)) else pc state + 4
        })

-- ID stage
instance Tickable MEMORY IF_ID ID_EX where
    tick :: MEMORY -> IF_ID -> (MEMORY, ID_EX)
    tick state input = 
        (state, IdEx {
            id_a = regs state !! fromIntegral (rs (if_ir input)), 
            id_b = regs state !! fromIntegral (rt (if_ir input)), 
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
                        IType -> compute code (id_a input) (zeroExtend (id_imm input)),
                    ex_b = 0
                })   
            LS -> 
                (state, ExMem{
                    ex_ir = id_ir input,
                    ex_alu_o = id_a input + zeroExtend (id_imm input),
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
                                alu_o = fromIntegral (id_pc input) + fromIntegral (id_imm input) * 4
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
            LS -> handleMEM state input
        where code = opcode (ex_ir input)

instance Tickable MEMORY MEM_WB (Maybe reg) where
    tick :: MEMORY -> MEM_WB -> (MEMORY, Maybe reg)
    tick state input | mem_ir input == 0 = (state, Nothing)
    tick state input =
        case instrCat code of
            Alu -> 
                (handleRegWrite state (fromIntegral (locationReg (mem_ir input))) (mem_alu_o input), Nothing)
                where locationReg = if writesFromRd code then rd else rt
            LS -> 
                if isLoad code then
                    (handleRegWrite state (fromIntegral (rt (mem_ir input))) (mem_lmd input), Nothing)
                else (state, Nothing)
        where code = opcode (mem_ir input) 


handleMEM :: MEMORY -> EX_MEM -> (MEMORY, MEM_WB)
handleMEM a b = (Memory{}, MemWb{})