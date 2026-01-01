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
booleanBuiltins = [ BackendBuiltins ("==", 2, pdpBinaryBuiltin "==" (~==))
            ,   BackendBuiltins ("eq", 2, pdpBinaryBuiltin "eq" (~==))
            ,   BackendBuiltins ("!=", 2, pdpBinaryBuiltin "!=" (~/=))
            ,   BackendBuiltins ("neq", 2, pdpBinaryBuiltin "neq" (~/=))
            ,   BackendBuiltins ("<", 2, pdpBinaryBuiltin "<" (~<))
            ,   BackendBuiltins ("lw", 2, pdpBinaryBuiltin "lw" (~<))
            ,   BackendBuiltins (">", 2, pdpBinaryBuiltin ">" (~>))
            ,   BackendBuiltins ("gt", 2, pdpBinaryBuiltin "gt" (~>))
            ,   BackendBuiltins ("<=", 2, pdpBinaryBuiltin "<=" (~<=))
            ,   BackendBuiltins ("lweq", 2, pdpBinaryBuiltin "lweq" (~<=))
            ,   BackendBuiltins (">=", 2, pdpBinaryBuiltin ">=" (~>=))
            ,   BackendBuiltins ("gteq", 2, pdpBinaryBuiltin "gteq" (~>=))
            ,   BackendBuiltins ("!", 1, pdpNot)
            ,   BackendBuiltins ("not", 1, pdpNot)
            ,   BackendBuiltins ("&&", 2, pdpBoolOperations "&&" (&&))
            ,   BackendBuiltins ("and", 2, pdpBoolOperations "and" (&&))
            ,   BackendBuiltins ("||", 2, pdpBoolOperations "||" (||))
            ,   BackendBuiltins ("or", 2, pdpBoolOperations "pr" (||))
            ,   BackendBuiltins ("!&", 2, pdpBoolOperations "!&" nand)
            ,   BackendBuiltins ("nand", 2, pdpBoolOperations "nand" nand)
            ,   BackendBuiltins ("!|", 2, pdpBoolOperations "!|" nor)
            ,   BackendBuiltins ("nor", 2, pdpBoolOperations "nor" nor)
            ,   BackendBuiltins (":|", 2, pdpBoolOperations ":|" xor)
            ,   BackendBuiltins ("xor", 2, pdpBoolOperations "xor" xor)
            ,   BackendBuiltins ("!:", 2, pdpBoolOperations "!:" xnor)
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
