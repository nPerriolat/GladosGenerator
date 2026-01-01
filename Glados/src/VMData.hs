module VMData (
    Address,
    Vm(..),
    addrToBytes,
    defaultVM
) where

import Data.Word (Word8, Word32)

import Any (Any)
import Bits (splitWord32)

type Address = Word32

data Vm = Vm {
    _registers :: [[Maybe Any]],    -- 16 registers
    _callStack :: [Address],    -- call stack for function calls
    _bf :: Maybe Bool,      -- boolean flag for branching
    _valueStack :: [Any],   -- value stack (where args are pushed)
    _pc :: Address          -- position of current opcode
} deriving Show

defaultVM :: Address -> Vm
defaultVM pc = Vm {
    _registers = [replicate 16 Nothing],
    _callStack = [],
    _bf = Nothing,
    _valueStack = [],
    _pc = pc
}

addrToBytes :: Address -> [Word8]
addrToBytes = splitWord32

