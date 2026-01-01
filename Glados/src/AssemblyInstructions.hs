{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module AssemblyInstructions (RegisterID, AssemblyInstruction(..), astToAny, assemble, toAssemblyValueInstruction, concatMapM, OutputAssemblyInstruction(..)) where

import Control.Applicative (liftA3)
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Word (Word8)

import Any (Any(..), AnyAssembly(..), makeAny, anyType)
import AST (AST(..))
import Bits (u32)
import Hex (showHex32)
import Serialize
import Type (Type(..))
import Utils (Safe(..), concatMapM)
import VMData (Address, addrToBytes)

type RegisterID = Word8

data AssemblyInstruction =  PushRegister RegisterID                                                 |
                            PushValue Any                                                           |
                            Pop RegisterID                                                          |
                            Test                                                                    |
                            Jump Address                                                            |
                            JumpIfTrue Address                                                      |
                            JumpIfFalse Address                                                     |
                            If [AssemblyInstruction] [AssemblyInstruction] [AssemblyInstruction]    |
                            Call String                                                             |
                            RetRegister RegisterID                                                  |
                            RetValue Any                                                            |
                            Ret                                                                     |
                            MovRegister RegisterID RegisterID                                       |
                            MovValue RegisterID Any                                                 |
                            OutRegister RegisterID                                                  |
                            OutValue Any                                                            |
                            Construct Type Int

data OutputAssemblyInstruction = OutputAssemblyInstruction AssemblyInstruction |
                                 Label String

instance Show OutputAssemblyInstruction where
    show :: OutputAssemblyInstruction -> String
    show (Label str) = str ++ ":"
    show (OutputAssemblyInstruction asm) = "\t" ++ show asm

instance Show AssemblyInstruction where
    show :: AssemblyInstruction -> String
    show (PushRegister reg) = "push r" ++ show reg
    show (PushValue val) = "push " ++ show (AnyAssembly val)
    show (Pop reg) = "pop r" ++ show reg
    show Test = "test"
    show (Construct _type n) = "construct " ++ show _type ++ " " ++ show n
    show (If cond trueCode falseCode) = "if\n" ++ concat (intersperse "\n" (map (("\t" ++) . show) cond)) ++ "\nthen\n" ++ concat (intersperse "\n" (map (("\t" ++) . show) falseCode)) ++ "\nelse\n" ++ concat (intersperse "\n" (map (("\t" ++) . show) trueCode)) ++ "\nend"
    show (Call symbol) = "call " ++ symbol
    show (RetRegister reg) = "ret r" ++ show reg
    show (RetValue val) = "ret " ++ show (AnyAssembly val)
    show Ret = "ret"
    show (MovRegister dest src) = "mov r" ++ show dest ++ ", " ++ show src
    show (MovValue dest val) = "mov r" ++ show dest ++ ", " ++ show (AnyAssembly val)
    show (OutRegister reg) = "out r" ++ show reg
    show (OutValue val) = "out " ++ show (AnyAssembly val)
    show (Jump jmp) = "jmp " ++ showHex32 jmp
    show (JumpIfFalse jmp) = "jf " ++ showHex32 jmp ++ " (instructions, not actual bytes)"
    show (JumpIfTrue jmp) = "jt " ++ showHex32 jmp ++ " (instructions, not actual bytes)"

astToAny :: AST -> Safe Any
astToAny (ASTBool b) = makeAny T_Bool b
astToAny (ASTChar c) = makeAny T_Char c
astToAny (ASTInt n) = makeAny T_Int n
astToAny (ASTUInt n) = makeAny T_UInt n
astToAny (ASTFloat f) = makeAny T_Float f
astToAny (ASTString str) = makeAny T_String str
astToAny (ASTArray xs) = mapM astToAny xs <&> Array
astToAny (ASTTuple (a, b)) = liftA2 (curry Tuple) (astToAny a) (astToAny b)
astToAny a = Error ("astToAny: Invalid argument : '" ++ show a ++ "'")

toAssemblyValueInstruction :: (Any -> AssemblyInstruction) -> AST -> Safe AssemblyInstruction
toAssemblyValueInstruction instruction ast = fmap instruction (astToAny ast)

assemble1 :: AssemblyInstruction -> Safe [Word8]
assemble1 (PushValue val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x00] ++) -- 0x00 : 1st nibble = instruction ID, 2nd nibble = addressing mode
assemble1 (PushRegister reg) = Value [0x01, reg]
assemble1 (Pop reg) = Value [0x10, reg]
assemble1 (Construct _type n) = liftA2 (++) (serializeType _type) (serialize n) <&> ([0x20] ++)
assemble1 Test = Value [0x30]
assemble1 (Call name) = concatMapM serialize name <&> (\bytes -> [0x60] ++ bytes ++ [0x00])
assemble1 (RetValue val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x70] ++)
assemble1 (RetRegister reg) = Value [0x71, reg]
assemble1 Ret = Value [0x72]
assemble1 (MovValue dest val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x80, dest] ++)
assemble1 (MovRegister dest src) = Value [0x81, dest, src]
assemble1 (OutValue val) = liftA2 (++) (anyType val >>= serializeType) (serialize val) <&> ([0x90] ++)
assemble1 (OutRegister reg) = Value [0x91, reg]
assemble1 (If cond trueCode falseCode) = liftA3 (
        \cond'' trueCode'' falseCode'' -> let skipFalse = [0xA0] ++ addrToBytes (u32 $ (length falseCode''))
                                          in cond'' ++ ([0x50] ++ addrToBytes (u32 $ length trueCode'' + length skipFalse)) ++ trueCode'' ++ skipFalse ++ falseCode''
    ) cond' trueCode' falseCode'
    where cond' = assemble cond
          trueCode' = assemble trueCode
          falseCode' = assemble falseCode

assemble1 a = Error ("assemble1: Instruction " ++ show a ++ " not implemented !")

assemble :: [AssemblyInstruction] -> Safe [Word8]
assemble = concatMapM assemble1
