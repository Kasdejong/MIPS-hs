module Memory where

import Data.Int
import Data.Bits
import Data.Binary
import Data.Sequence
import Prelude hiding ((!!), length, lookup, take)

import Util

data MEMORY = Memory {mem :: Seq Word8, regs :: Seq Word32, wires :: WIRES, pc :: Int} deriving (Show, Eq)
data WIRES = Wires {cond :: Bool, alu_o :: Word32} deriving (Show, Eq)

handleMemWrite :: MEMORY -> Int -> Int -> Word32 -> MEMORY
handleMemWrite oldMem idx nBytes input = 
    Memory{
        regs = regs oldMem, 
        wires = wires oldMem, 
        pc = pc oldMem, 
        mem = writeToByteArr (mem oldMem) idx nBytes (getByteSeq input)
    }

(!!) :: Seq a -> Int -> a
(!!) seq idx = forceExtractMaybe (lookup idx seq)

readMemWord :: Int -> Seq Word8 -> Word32
readMemWord idx mem = 
    let b0 = fromIntegral (mem !! idx)
        b1 = fromIntegral (mem !! (idx + 1))
        b2 = fromIntegral (mem !! (idx + 2))
        b3 = fromIntegral (mem !! (idx + 3))
    in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3

handleRegWrite :: MEMORY -> Int -> Word32 -> MEMORY
handleRegWrite oldMem idx input =
    Memory{
        wires = wires oldMem,
        pc = pc oldMem,
        mem = mem oldMem,
        regs = update idx input (regs oldMem) 
    }

handleLoRegWrite :: MEMORY -> Int -> Word16 -> MEMORY
handleLoRegWrite oldMem idx input = 
    Memory{
        wires = wires oldMem,
        pc = pc oldMem,
        mem = mem oldMem,
        regs = update idx ((0xFFFF0000 .|. zeroExtend input) .&. forceExtractMaybe (lookup idx (regs oldMem))) (regs oldMem) 
    }


forceExtractMaybe :: Maybe a -> a
forceExtractMaybe (Just val) = val
forceExtractMaybe Nothing = error "huh?"

writeToByteArr :: Seq Word8 -> Int -> Int -> Seq Word8 -> Seq Word8
writeToByteArr mem _ 0 _ = mem 
writeToByteArr mem idx nBytes input = 
    writeToByteArr 
        (update idx (forceExtractMaybe (lookup lenMinus1 input)) mem) 
        (idx + 1) 
        (nBytes - 1) 
        (take lenMinus1 input)
    where lenMinus1 = length input - 1

