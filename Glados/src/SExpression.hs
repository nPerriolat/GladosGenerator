module SExpression
    ( SExpr(SNumber, SSymbol, SList, STuple, SArray, SFunctionType, SString, SFloat, SUint, SChar),
    getSymbol,
    getInteger,
    getList,
    fromSymbol
    ) where

import Utils

data SExpr =    SNumber Int             | 
                SSymbol String          |
                SList [SExpr]           |
                STuple [SExpr]          |
                SArray [SExpr]          |
                SFunctionType [SExpr]   |
                SString String          |
                SFloat Float            |
                SUint Int               |
                SChar Char
                deriving (Eq, Show)

getSymbol :: SExpr -> Safe String
getSymbol (SSymbol symbol) = Value symbol
getSymbol _ = Error "SExpr is not a SSymbol."

getInteger :: SExpr -> Safe Int
getInteger (SNumber n) = Value n
getInteger _ = Error "SExpr is not a SNumber."

getList :: SExpr -> Safe [SExpr]
getList (SList exprs) = Value exprs
getList _ = Error "SExpr is not a SList."

fromSymbol :: SExpr -> String
fromSymbol (SSymbol s) = s
fromSymbol _ = ""
