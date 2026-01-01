module BooleanOperator(booleanBuiltins) where

import Any
import DataBuiltins (Symbols, BuiltinsSymbol(BackendBuiltins))
import Utils (Safe(..))

nand :: Bool -> Bool -> Bool
nand a b = not $ a && b

nor :: Bool -> Bool -> Bool
nor a b = not $ a || b

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

xnor :: Bool -> Bool -> Bool
xnor a b = not $ xor a b

booleanBuiltins :: Symbols
booleanBuiltins = [ BackendBuiltins ("EQ_OP", 2, pdpBinaryBuiltin "EQ_OP" (~==))
            ,   BackendBuiltins ("eq", 2, pdpBinaryBuiltin "eq" (~==))
            ,   BackendBuiltins ("NEQ_OP", 2, pdpBinaryBuiltin "NEQ_OP" (~/=))
            ,   BackendBuiltins ("neq", 2, pdpBinaryBuiltin "neq" (~/=))
            ,   BackendBuiltins ("LW_OP", 2, pdpBinaryBuiltin "LW_OP" (~<))
            ,   BackendBuiltins ("lw", 2, pdpBinaryBuiltin "lw" (~<))
            ,   BackendBuiltins ("GT_OP", 2, pdpBinaryBuiltin "GT_OP" (~>))
            ,   BackendBuiltins ("gt", 2, pdpBinaryBuiltin "gt" (~>))
            ,   BackendBuiltins ("LWEQ_OP", 2, pdpBinaryBuiltin "LWEQ_OP" (~<=))
            ,   BackendBuiltins ("lweq", 2, pdpBinaryBuiltin "lweq" (~<=))
            ,   BackendBuiltins ("GTEQ_OP", 2, pdpBinaryBuiltin "GTEQ_OP" (~>=))
            ,   BackendBuiltins ("gteq", 2, pdpBinaryBuiltin "gteq" (~>=))
            ,   BackendBuiltins ("NOT_OP", 1, pdpNot)
            ,   BackendBuiltins ("not", 1, pdpNot)
            ,   BackendBuiltins ("AND_OP", 2, pdpBoolOperations "AND_OP" (&&))
            ,   BackendBuiltins ("and", 2, pdpBoolOperations "and" (&&))
            ,   BackendBuiltins ("OR_OP", 2, pdpBoolOperations "OR_OP" (||))
            ,   BackendBuiltins ("or", 2, pdpBoolOperations "pr" (||))
            ,   BackendBuiltins ("NAND_OP", 2, pdpBoolOperations "NAND_OP" nand)
            ,   BackendBuiltins ("nand", 2, pdpBoolOperations "nand" nand)
            ,   BackendBuiltins ("NOR_OP", 2, pdpBoolOperations "NOR_OP" nor)
            ,   BackendBuiltins ("nor", 2, pdpBoolOperations "nor" nor)
            ,   BackendBuiltins ("XOR_OP", 2, pdpBoolOperations "XOR_OP" xor)
            ,   BackendBuiltins ("xor", 2, pdpBoolOperations "xor" xor)
            ,   BackendBuiltins ("XNOR_OP", 2, pdpBoolOperations "XNOR_OP" xnor)
            ,   BackendBuiltins ("xnor", 2, pdpBoolOperations "xnor" xnor)]
            -- ,   BackendBuiltins ("if", pdpIf)]

pdpBinaryBuiltin :: String -> (Any -> Any -> Safe Any) -> [Any] -> Safe Any
pdpBinaryBuiltin _ f [a, b] = f a b
pdpBinaryBuiltin name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only compare 2 arguments, but got " ++ show (length args) ++ " !")

-- pdpIf' :: Symbols -> [Any] -> Safe Any
-- pdpIf' symbols [(Bool condition), a, b] = if condition then eval a else eval b
--     where eval s = fst (evaluateAny1 symbols s)
-- pdpIf' _ args = Error ("if must be called as 'if <condition as boolean> <a> <b>', but got args " ++ show args)

-- pdpIf :: Symbols -> [Any] -> Safe Any
-- pdpIf symbols [a, b, c] = (fst $ evaluateAny1 symbols a) >>= (\a' -> pdpIf' symbols [a', b, c])
-- pdpIf _ args = Error ("if must be called with 3 arguments, but got " ++ show (length args))

pdpBoolOperations :: String -> (Bool -> Bool -> Bool) -> [Any] -> Safe Any
pdpBoolOperations _ f [Bool a, Bool b] = Value $ Bool (f a b)
pdpBoolOperations name _ args = Error ("Bad arguments when attempting to call " ++ name ++ " ! Expected 2 boolean but got " ++ show args ++ " !")

pdpNot :: [Any] -> Safe Any
pdpNot [Bool a] = Value $ Bool $ not a
pdpNot args = Error ("Bad arguments when attempting to call '!' ! Expected a boolean but got " ++ show args ++ " !")
