module Mips.Util where 

import Data.Bits
import Data.Int
import Data.Binary
import Data.Sequence
-- offset 0 = least significant bit
getFromIr :: Int -> Int -> Word32 -> Word32
getFromIr offset len bytes = shiftR bytes offset .&. 2^len

extractBytes :: Word32 -> [Word8]
extractBytes w =
  [ fromIntegral (shiftR w 24) .&. 0xFF
  , fromIntegral (shiftR w 16) .&. 0xFF
  , fromIntegral (shiftR w 8)  .&. 0xFF
  , fromIntegral w             .&. 0xFF
  ]

zeroExtend :: Word16 -> Word32
zeroExtend = fromIntegral

getByteSeq :: Word32 -> Seq Word8
getByteSeq word = fromList (extractBytes word)

opcode :: Word32 -> Word8
opcode word = fromIntegral (getFromIr 26 6 word)

rs :: Word32 -> Word32
rs word = fromIntegral (getFromIr 21 5 word)

rt :: Word32 -> Word8
rt word = fromIntegral (getFromIr 16 5 word)

rd :: Word32 -> Word8
rd word = fromIntegral (getFromIr 11 5 word)

shamt :: Word32 -> Word8
shamt word = fromIntegral (getFromIr 6 5 word)

funct :: Word32 -> Word8
funct word = fromIntegral (getFromIr 0 6 word)

imm :: Word32 -> Word16
imm word = fromIntegral (getFromIr 0 16 word)

addr :: Word32 -> Word32
addr word = fromIntegral (getFromIr 0 26 word)
