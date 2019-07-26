module CodeGen(generateCode, Instruction) where

import Control.Monad.State
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Bits
import Data.Word

import qualified IR

type Instruction = Integer
type Line = Integer
type BackpatchTable = Map.Map IR.Label Line

generateCode :: [IR.Line] -> [Instruction]
generateCode ir =
    let bpt = mkBackpatchTable ir 0 (Map.empty)
    in genCode ir bpt

mkBackpatchTable :: [IR.Line] -> Line -> BackpatchTable -> BackpatchTable
mkBackpatchTable [] _ bpt = bpt
mkBackpatchTable ((IR.Line label cmd):xs) l bpt =
    case label of
        (Just s) -> let bpt' = Map.insert s l bpt
                    in mkBackpatchTable xs (succ l) bpt'
        Nothing  -> mkBackpatchTable xs (succ l) bpt

genCode :: [IR.Line] -> BackpatchTable -> [Instruction]
genCode [] _ = []
genCode ((IR.Line label (IR.JMP l)):xs) bpt =
    let (Just address) = Map.lookup l bpt
    in (0x01000000 .|. address):(genCode xs bpt)

genCode ((IR.Line label (IR.JMZ l)):xs) bpt =
    let (Just address) = Map.lookup l bpt
    in (0x02000000 .|. address):(genCode xs bpt)

genCode ((IR.Line label (IR.LOAD l)):xs) bpt =
    let (Just address) = Map.lookup l bpt
    in (0x03000000 .|. address):(genCode xs bpt)

genCode ((IR.Line label (IR.STORE l)):xs) bpt =
    let (Just address) = Map.lookup l bpt
    in (0x04000000 .|. address):(genCode xs bpt)

genCode ((IR.Line _ IR.WRITE):xs) bpt = 0x05000000:(genCode xs bpt)
genCode ((IR.Line _ IR.ADD):xs) bpt = 0x06000000:(genCode xs bpt)
genCode ((IR.Line _ IR.SUB):xs) bpt = 0x07000000:(genCode xs bpt)
genCode ((IR.Line _ IR.DIV):xs) bpt = 0x08000000:(genCode xs bpt)
genCode ((IR.Line _ IR.MUL):xs) bpt = 0x09000000:(genCode xs bpt)
genCode ((IR.Line _ IR.LT):xs) bpt = 0x0A000000:(genCode xs bpt)
genCode ((IR.Line _ IR.LTE):xs) bpt = 0x0B000000:(genCode xs bpt)
genCode ((IR.Line _ IR.GT):xs) bpt = 0x0C000000:(genCode xs bpt)
genCode ((IR.Line _ IR.GTE):xs) bpt = 0x0D000000:(genCode xs bpt)
genCode ((IR.Line _ IR.EQ):xs) bpt = 0x0E000000:(genCode xs bpt)
genCode ((IR.Line _ IR.NEQ):xs) bpt = 0x0F000000:(genCode xs bpt)
genCode ((IR.Line _ IR.HALT):xs) bpt = 0x1F000000:(genCode xs bpt)
genCode ((IR.Line _ IR.STARTFUNC):xs) bpt = 0x2F000000:(genCode xs bpt)
genCode ((IR.Line _ IR.NOOP):xs) bpt= 0x2F000000:(genCode xs bpt)
genCode ((IR.Line _ IR.RET):xs) bpt = 0x3F000000:(genCode xs bpt)
genCode ((IR.Line _ (IR.DEC i)):xs) bpt = i:(genCode xs bpt)

genCode ((IR.Line label (IR.CALL l)):xs) bpt =
    let (Just address) = Map.lookup l bpt
    in (0x4F000000 .|. address):(genCode xs bpt)

