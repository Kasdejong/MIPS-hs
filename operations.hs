{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Operations where
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
op_add     = 0b100000
op_addu    = 0b100001
op_addi    = 0b001000
op_addiu   = 0b001001
op_and     = 0b100100
op_andi    = 0b001100
op_div     = 0b011010
op_divu    = 0b011011
op_mult    = 0b011000
op_multu   = 0b011001
op_nor     = 0b100111
op_or      = 0b100101
op_ori     = 0b001101
op_sll     = 0b000000
op_sllv    = 0b000100
op_sra     = 0b000011
op_srav    = 0b000111
op_srl     = 0b000010
op_srlv    = 0b000110
op_sub     = 0b100010
op_subu    = 0b100011
op_xor     = 0b100110
op_xori    = 0b001110
op_lhi     = 0b011001
op_llo     = 0b011000

op_slt     = 0b101010
op_sltu    = 0b101001
op_slti    = 0b001010
op_sltiu   = 0b001001
op_beq     = 0b000100
op_bgtz    = 0b000111
op_blez    = 0b000110
op_bne     = 0b000101
op_j       = 0b000010
op_jal     = 0b000011
op_jalr    = 0b001001
op_jr      = 0b001000
op_lb      = 0b100000
op_lbu     = 0b100100
op_lh      = 0b100001
op_lhu     = 0b100101
op_lw      = 0b100011
op_sb      = 0b101000
op_sh      = 0b101001
op_sw      = 0b101011
op_mfhi    = 0b010000
op_mflo    = 0b010010
op_mthi    = 0b010001
op_mtlo    = 0b010011
op_trap    = 0b011010

-- Function to map Int (opcode) to OpKind
opKind :: Int -> OpKind
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

-- Function to map Int (opcode) to InstrCategory
instrCat :: Int -> InstrCategory
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

encodingType :: Int -> InstrEncoding
encodingType opcode
    -- R-type instructions (Register based)
    | opKind opcode `elem` [ArithLog, DivMult, Shift, ShiftV, MoveFrom, MoveTo] = RType
    -- I-type instructions (Immediate based)
    | opKind opcode `elem` [ArithLogI, LoadI, Branch, BranchZ, LoadStore] = IType
    -- J-type instructions (Jump based)
    | opKind opcode `elem` [Jump, Trap] = JType
    | otherwise = error "Unknown OpKind"
