import Test.HUnit
import qualified System.Exit as Exit
import CPU
import Memory
import Data.Binary
import Data.Sequence

main :: IO()
main = do
    result <- runTestTT testIfStage
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

startState = Memory{pc = 0, mem = fromList [0x21, 0x34, 0x56, 0x78, 0x01, 0x00, 0x00, 0x00], wires = Wires{cond = False, alu_o = 0}, regs = fromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]}

testIfStage :: Test
testIfStage = do
    let actual = tick startState Nothing
    let expectedIr = 0x21345678
    let expected = (startState, IfId{if_ir = expectedIr, if_pc = 4})
    TestCase(assertEqual "if stage error" expected actual)
