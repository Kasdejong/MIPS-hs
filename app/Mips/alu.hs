module Mips.Alu where

import Data.Binary

compute :: Word8 -> Word32 -> Word32 -> Word32
compute opcode a b = 0