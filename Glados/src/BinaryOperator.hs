module BinaryOperator(binaryBuiltins) where

import Data.Bits
import Data.Char (ord, chr)

import Any (Any(..))
import Bits (word, int)
import DataBuiltins (Symbols, BuiltinsSymbol(BackendBuiltins))
import Utils (Safe(..))

wordifyShift :: (Word -> Int -> Word) -> (Word -> Word -> Word)
wordifyShift f = (\a b -> f a (int b))

binaryBuiltins :: Symbols
binaryBuiltins = [ BackendBuiltins ("BAND_OP", 2, pdpBinaryOp "BAND_OP" (.&.))
            ,   BackendBuiltins ("band", 2, pdpBinaryOp "band" (.&.))
            ,   BackendBuiltins ("BOR_OP", 2, pdpBinaryOp "BOR_OP" (.|.))
            ,   BackendBuiltins ("bor", 2, pdpBinaryOp "bor" (.|.))
            ,   BackendBuiltins ("BNOT_OP", 1, pdpBNot)
            ,   BackendBuiltins ("bnot", 1, pdpBNot)
            ,   BackendBuiltins ("BXOR_OP", 2, pdpBinaryOp "BXOR_OP" xor)
            ,   BackendBuiltins ("bxor", 2, pdpBinaryOp "bxor" xor)
            ,   BackendBuiltins ("RSHIFT_OP", 2, pdpBinaryOp "RSHIFT_OP" (wordifyShift (.>>.)))
            ,   BackendBuiltins ("rshift", 2, pdpBinaryOp ">>" (wordifyShift (.>>.)))
            ,   BackendBuiltins ("LSHIFT_OP", 2, pdpBinaryOp "LSHIFT_OP" (wordifyShift (.<<.)))
            ,   BackendBuiltins ("lshift", 2, pdpBinaryOp "<<" (wordifyShift (.<<.)))]


pdpBinaryOp :: String -> (Word -> Word -> Word) -> [Any] -> Safe Any
pdpBinaryOp _ f [Int a, Int b] = Value $ Int $ int $ f (word a) (word b)
pdpBinaryOp _ f [Int a, UInt b] = Value $ UInt $ f (word a) b
pdpBinaryOp _ f [Int a, Char b] = Value $ Int $ int $ f (word a) (word $ ord b)
pdpBinaryOp _ f [UInt a, UInt b] = Value $ UInt $ f a b
pdpBinaryOp _ f [Char a, UInt b] = Value $ UInt $ f (word $ ord a) b
pdpBinaryOp _ f [Char a, Char b] = Value $ Char $ chr $ int $ f (word $ ord a) (word $ ord b)
pdpBinaryOp name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 integers but got " ++ show args ++ " !")

pdpBNot :: [Any] -> Safe Any
pdpBNot [Int a] = Value $ Int $ complement a
pdpBNot [UInt a] = Value $ UInt $ complement a
pdpBNot [Char a] = Value $ Char $ chr $ complement (ord a)
pdpBNot args = Error ("Bad arguments when attempting to call '~' ! Expected an integer but got " ++ show args ++ " !")
