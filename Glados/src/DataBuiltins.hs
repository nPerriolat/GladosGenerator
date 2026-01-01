module DataBuiltins (
    BuiltinsSymbol(..),
    Symbols
) where

import Any (Any)
import Utils(Safe)

data BuiltinsSymbol = BackendBuiltins (String, Int, [Any] -> Safe Any)
type Symbols = [BuiltinsSymbol]