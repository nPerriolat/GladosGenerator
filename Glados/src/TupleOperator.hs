module TupleOperator (tupleBuiltins) where

import Data.Tuple (swap)

import Any
import DataBuiltins (Symbols, BuiltinsSymbol (BackendBuiltins))
import Utils (Safe(..))

left :: (Any, Any) -> Safe Any
left = Value .fst

right :: (Any, Any) -> Safe Any
right = Value . snd

swap' :: (Any, Any) -> Safe Any
swap' = Value . Tuple . swap

pdpTupleBuiltin :: String -> ((Any, Any) -> Safe Any) -> [Any] -> Safe Any
pdpTupleBuiltin _ f [(Tuple tuple)] = f tuple
pdpTupleBuiltin name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only take one tuple argument, but got " ++ show (length args) ++ " !")

tupleBuiltins :: Symbols
tupleBuiltins = [ BackendBuiltins ("left", 1, pdpTupleBuiltin "left" left)
                , BackendBuiltins ("right", 1, pdpTupleBuiltin "right" right)
                , BackendBuiltins ("swap", 1, pdpTupleBuiltin "swap" swap') ]