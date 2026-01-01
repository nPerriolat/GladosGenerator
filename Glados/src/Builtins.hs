module Builtins(builtins) where

import BinaryOperator (binaryBuiltins)
import BooleanOperator (booleanBuiltins)
import DataBuiltins (Symbols)
import ListOperator (listBuiltins)
import MathLib (mathBuiltins)
import TupleOperator (tupleBuiltins)

builtins :: Symbols
builtins = mathBuiltins ++ booleanBuiltins ++ binaryBuiltins ++ tupleBuiltins ++ listBuiltins