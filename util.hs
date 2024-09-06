module Util where 

import Data.Bits
-- offset 0 = least significant bit
getFromIr :: Int -> Int -> Int -> Int
getFromIr offset len bytes = shiftR bytes offset .&. 2^len

opcode :: Int -> Int
opcode = getFromIr 26 6

rs :: Int -> Int
rs = getFromIr 21 5

rt :: Int -> Int
rt = getFromIr 16 5

rd :: Int -> Int
rd = getFromIr 11 5

shamt :: Int -> Int
shamt = getFromIr 6 5

funct :: Int -> Int
funct = getFromIr 0 6

imm :: Int -> Int
imm = getFromIr 0 16

addr :: Int -> Int
addr = getFromIr 0 26
