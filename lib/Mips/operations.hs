{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Operations where
import Data.Binary
-- Define the OpKind data type with subtypes based on the table
-- Define the OpKind data type
data OpKind = ArithLog | ArithLogI | DivMult | Shift | ShiftV | Branch | BranchZ | Jump | JumpR | LoadStore | MoveFrom | MoveTo | Trap | LoadI
    deriving (Enum, Eq, Show)

-- Define the InstrCategory data type
data InstrCategory = Br | LS | Alu
    deriving (Enum, Eq, Show)

-- Define the EncodingType data type
data InstrEncoding = RType | IType | JType
    deriving (Enum, Eq, Show)

-- Define all opcodes as constants
op_add     = 0x20 :: Word8
op_addu    = 0x21 :: Word8
op_addi    = 0x08 :: Word8
op_addiu   = 0x09 :: Word8
op_and     = 0x24 :: Word8
op_andi    = 0x0C :: Word8
op_div     = 0x34 :: Word8
op_divu    = 0x35 :: Word8
op_mult    = 0x18 :: Word8
op_multu   = 0x19 :: Word8
op_nor     = 0x27 :: Word8
op_or      = 0x25 :: Word8
op_ori     = 0x0D :: Word8
op_sll     = 0x00 :: Word8
op_sllv    = 0x04 :: Word8
op_sra     = 0x03 :: Word8
op_srav    = 0x07 :: Word8
op_srl     = 0x02 :: Word8
op_srlv    = 0x06 :: Word8
op_sub     = 0x22 :: Word8
op_subu    = 0x23 :: Word8
op_xor     = 0x26 :: Word8
op_xori    = 0x0E :: Word8
op_lhi     = 0x19 :: Word8
op_llo     = 0x18 :: Word8
op_slt     = 0x2A :: Word8
op_sltu    = 0x2B :: Word8
op_slti    = 0x0A :: Word8
op_sltiu   = 0x09 :: Word8
op_beq     = 0x04 :: Word8
op_bgtz    = 0x07 :: Word8
op_blez    = 0x06 :: Word8
op_bne     = 0x05 :: Word8
op_j       = 0x02 :: Word8
op_jal     = 0x03 :: Word8
op_jalr    = 0x09 :: Word8
op_jr      = 0x08 :: Word8
op_lb      = 0x20 :: Word8
op_lbu     = 0x24 :: Word8
op_lh      = 0x21 :: Word8
op_lhu     = 0x25 :: Word8
op_lw      = 0x23 :: Word8
op_sb      = 0x28 :: Word8
op_sh      = 0x29 :: Word8
op_sw      = 0x2B :: Word8
op_mfhi    = 0x10 :: Word8
op_mflo    = 0x12 :: Word8
op_mthi    = 0x11 :: Word8
op_mtlo    = 0x13 :: Word8
op_trap    = 0x34 :: Word8



opKind :: Word8 -> OpKind
opKind opcode
    | opcode `elem` [op_add, op_addu, op_and, op_nor, op_sub, op_subu, op_slt, op_sltu, op_or, op_xor] = ArithLog
    | opcode `elem` [op_addi, op_addiu, op_andi, op_ori, op_slti, op_sltiu, op_xori, op_lhi, op_llo] = ArithLogI
    | opcode `elem` [op_div, op_divu, op_mult, op_multu] = DivMult
    | opcode `elem` [op_sll, op_sra, op_srl] = Shift
    | opcode `elem` [op_sllv, op_srav, op_srlv] = ShiftV
    | opcode `elem` [op_beq, op_bne] = Branch
    | opcode `elem` [op_bgtz, op_blez] = BranchZ
    | opcode `elem` [op_j, op_jal] = Jump
    | opcode `elem` [op_jalr, op_jr] = JumpR
    | opcode `elem` [op_lb, op_lbu, op_lh, op_lhu, op_lw, op_sb, op_sh, op_sw] = LoadStore
    | opcode `elem` [op_mfhi, op_mflo] = MoveFrom
    | opcode `elem` [op_mthi, op_mtlo] = MoveTo
    | opcode == op_trap = Trap
    | otherwise = error "Unknown opcode"


instrCat :: Word8 -> InstrCategory
instrCat opcode
    -- Branches and Jumps
    | opcode `elem` [op_beq, op_bne, op_bgtz, op_blez, op_j, op_jal, op_jr, op_jalr] = Br
    -- Loads and Stores
    | opcode `elem` [op_lb, op_lbu, op_lh, op_lhu, op_lw, op_sb, op_sh, op_sw] = LS
    -- ALU Operations
    | opcode `elem` [op_add, op_addu, op_and, op_nor, op_sub, op_subu, op_slt, op_sltu,
                     op_addi, op_addiu, op_andi, op_ori, op_slti, op_sltiu, op_xori,
                     op_sll, op_sra, op_srl, op_sllv, op_srav, op_srlv,
                     op_div, op_divu, op_mult, op_multu, op_mfhi, op_mflo, op_mthi, op_mtlo, op_trap,
                     op_lhi, op_llo] = Alu
    | otherwise = error "Unknown opcode"


encodingType :: Word8 -> InstrEncoding
encodingType opcode
    -- R-type instructions (Register based)
    | opKind opcode `elem` [ArithLog, DivMult, Shift, ShiftV, MoveFrom, MoveTo] = RType
    -- I-type instructions (Immediate based)
    | opKind opcode `elem` [ArithLogI, LoadI, Branch, BranchZ, LoadStore] = IType
    -- J-type instructions (Jump based)
    | opKind opcode `elem` [Jump, Trap] = JType
    | otherwise = error "Unknown OpKind"


isLoad :: Word8 -> Bool
isLoad opcode
    | opcode `elem` [op_lb, op_lbu, op_lh, op_lhi, op_lhu, op_llo, op_lw] = True
    | otherwise = False

writesFromRd :: Word8 -> Bool
writesFromRd opcode = True