module MathLib (mathBuiltins) where

import Data.Char(ord, chr)

import Any (Any(..))
import Bits (word)
import DataBuiltins (Symbols, BuiltinsSymbol(BackendBuiltins))
import Utils (Safe(..))

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]

mathBuiltins :: Symbols
mathBuiltins = [    BackendBuiltins ("MUL_OP", 2, pdpArithmeticOp "MUL_OP" (*))
            ,       BackendBuiltins ("mul", 2, pdpArithmeticOp "mul" (*))
            ,   BackendBuiltins ("ADD_OP", 2, pdpArithmeticOp "ADD_OP" (+))
            ,   BackendBuiltins ("add", 2, pdpArithmeticOp "add" (+))
            ,   BackendBuiltins ("SUB_OP", 2, pdpArithmeticOp "SUB_OP" (-))
            ,   BackendBuiltins ("sub", 2, pdpArithmeticOp "sub" (-))
            ,   BackendBuiltins ("div", 2, pdpArithmeticOp "div" (/))
            ,   BackendBuiltins ("DIV_OP", 2, pdpArithmeticOp "div" (/))
            ,   BackendBuiltins ("mod", 2, pdpModulo)
            ,   BackendBuiltins ("MOD_OP", 2, pdpModulo)
            ,   BackendBuiltins ("POW_OP", 2, pdpArithmeticOp "POW_OP" (**))
            ,   BackendBuiltins ("pow", 2, pdpArithmeticOp "pow" (**))
            ,   BackendBuiltins ("SQRT_OP", 1, pdpArithmeticSingleOp "SQRT_OP" sqrt)
            ,   BackendBuiltins ("sqrt", 1, pdpArithmeticSingleOp "SQRT_OP" sqrt)
            ,   BackendBuiltins ("FACTORIAL_OP", 1, pdpFactorial)
            ,   BackendBuiltins ("factorial", 1, pdpFactorial)
            ,   BackendBuiltins ("pi", 0, const (Value (Float 3.14159265)))
            ,   BackendBuiltins ("e", 0, const (Value (Float 2.71828182))) -- verifié liste parametre
            ,   BackendBuiltins ("exp", 1, pdpArithmeticSingleOp "exp" exp)
            ,   BackendBuiltins ("ln", 1, pdpArithmeticSingleOp "ln" log)
            ,   BackendBuiltins ("max", 2, pdpArithmeticOp "max" max)
            ,   BackendBuiltins ("min", 2, pdpArithmeticOp "min" min)
            ,   BackendBuiltins ("cos", 1, pdpArithmeticSingleOp "cos" cos)
            ,   BackendBuiltins ("acos", 1, pdpArithmeticSingleOp "acos" acos)
            ,   BackendBuiltins ("cosh", 1, pdpArithmeticSingleOp "cosh" cosh)
            ,   BackendBuiltins ("sin", 1, pdpArithmeticSingleOp "sin" sin)
            ,   BackendBuiltins ("asin", 1, pdpArithmeticSingleOp "asin" asin)
            ,   BackendBuiltins ("sinh", 1, pdpArithmeticSingleOp "sinh" sinh)
            ,   BackendBuiltins ("tan", 1, pdpArithmeticSingleOp "tan" tan)
            ,   BackendBuiltins ("atan", 1, pdpArithmeticSingleOp "atan" atan)
            ,   BackendBuiltins ("ceil", 1, pdpRounding "ceil" ceiling)
            ,   BackendBuiltins ("round", 1, pdpRounding "round" round)
            ,   BackendBuiltins ("trunc", 1, pdpRounding "trunc" truncate)
            ,   BackendBuiltins ("floor", 1, pdpRounding "floor" floor)]

pdpArithmeticOp :: String -> (Float -> Float -> Float) -> [Any] -> Safe Any
pdpArithmeticOp _ f [Float a, Float b] = Value $ Float (f a b)
pdpArithmeticOp _ f [Int a, Float b] = Value $ Float (f (fromIntegral a) b)
pdpArithmeticOp _ f [UInt a, Float b] = Value $ Float (f (fromIntegral a) b)
pdpArithmeticOp _ f [Char a, Float b] = Value $ Float (f (fromIntegral (ord a)) b)
pdpArithmeticOp _ f [Int a, Int b] = Value $ Int $ floor (f (fromIntegral a) (fromIntegral b))
pdpArithmeticOp _ f [Int a, UInt b] = Value $ Int $ floor (f (fromIntegral a) (fromIntegral b))
pdpArithmeticOp _ f [Int a, Char b] = Value $ Int $ floor (f (fromIntegral a) (fromIntegral (ord b)))
pdpArithmeticOp _ f [UInt a, UInt b] = Value $ UInt $ floor (f (fromIntegral a) (fromIntegral b))
pdpArithmeticOp _ f [Char a, UInt b] = Value $ UInt $ floor (f (fromIntegral (ord a)) (fromIntegral b))
pdpArithmeticOp _ f [Char a, Char b] = Value $ Char $ chr $ floor (f (fromIntegral (ord a)) (fromIntegral (ord b)))
pdpArithmeticOp name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 numbers but got " ++ show args ++ " !")

pdpModulo :: [Any] -> Safe Any
pdpModulo [Int a, Int b] = Value $ Int (mod a b)
pdpModulo [Int a, UInt b] = Value $ UInt (mod (word a) b)
pdpModulo [Int a, Char b] = Value $ Int (mod a (ord b))
pdpModulo [UInt a, UInt b] = Value $ UInt (mod a b)
pdpModulo [UInt a, Char b] = Value $ UInt (mod a (word $ ord b))
pdpModulo [Char a, Char b] = Value $ Char $ chr $ mod (ord a) (ord b)
pdpModulo args = Error ("Bad arguments when attempting to call '%'! expected an integer but got " ++ show args ++ " !")

pdpFactorial :: [Any] -> Safe Any
pdpFactorial [Int a]
                        | a >= 0 = Value $ UInt (word $ factorial a)
                        | otherwise = Error ("Bad arguments when attempting to call '!'! expected a positive integer but got " ++ show a ++ " !")
pdpFactorial [UInt a] = Value $ UInt (factorial a)
pdpFactorial [Char a] = Value $ UInt (word $ factorial (ord a))
pdpFactorial args = Error ("Bad arguments when attempting to call '!!'! expected an integer but got " ++ show args ++ " !")


pdpArithmeticSingleOp :: String -> (Float -> Float) -> [Any] -> Safe Any
pdpArithmeticSingleOp _ f [Float a] = Value $ Float (f a)
pdpArithmeticSingleOp _ f [Int a] = Value $ Float $ f (fromIntegral a)
pdpArithmeticSingleOp _ f [UInt a] = Value $ Float $ f (fromIntegral a)
pdpArithmeticSingleOp _ f [Char a] = Value $ Float $ f (fromIntegral (ord a))
pdpArithmeticSingleOp name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a number but got " ++ show args ++ " !")

pdpRounding :: String -> (Float -> Int) -> [Any] -> Safe Any
pdpRounding _ f [Float a] = Value $ Float $ fromIntegral (f a)
pdpRounding name _ args = Error ("Bad arguments when attempting to call " ++ name ++ "! expected a float but got " ++ show args ++ " !")
