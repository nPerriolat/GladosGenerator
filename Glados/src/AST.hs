module AST (
    Parameter,
    Call(..),
    AST(..),
    getTypeFunctionCall,
    getTypeAST
    ) where

import Type

type Parameter = (AST, Type)

data Call = LambdaCall [Parameter] AST Type | FunctionCall String deriving (Eq, Show)

-- ASTBoolean devient ASTBool ; ASTSymbol devient ASTProcedure
data AST =  ASTInt Int                              |
            ASTUInt Int                             |
            ASTChar Char                            |
            ASTFloat Float                          |
            ASTBool Bool                            |
            ASTTuple (AST, AST)                     |
            ASTArray [AST]                          |
            ASTString String                        |
            ASTProcedure String                     |
            ASTDefine String Type AST               |
            ASTFunction String [Parameter] AST Type |
            ASTLambda [Parameter] AST Type          |
            ASTCall Call [AST]                      |
            ASTIf AST AST AST                       |
            ASTNULL deriving (Eq, Show)

getTypeProcedure :: String -> [AST] -> Type
getTypeProcedure "e" _ = T_Float
getTypeProcedure "pi" _ = T_Float
getTypeProcedure _ [] = T_Undefined
getTypeProcedure procedure (ASTDefine name t _:rest)
    | procedure == name = t
    | otherwise = getTypeProcedure procedure rest
getTypeProcedure procedure (_:rest) = getTypeProcedure procedure rest

getTypeFunctionCall :: String -> [AST] -> Type
getTypeFunctionCall "ADD_OP" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "add" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "SUB_OP" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "sub" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "MUL_OP" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "mul" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "DIV_OP" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "div" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "MOD_OP" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "mod" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "SQRT_OP" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "sqrt" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "FACTORIAL_OP" _ = T_Function [typeInteger] T_UInt
getTypeFunctionCall "factorial" _ = T_Function [typeInteger] T_UInt
getTypeFunctionCall "ADD_ASSIGN_OP" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "add=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "SUB_ASSIGN_OP" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "sub=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "MUL_ASSIGN_OP" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "mul=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "DIV_ASSIGN_OP" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "div=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "MOD_ASSIGN_OP" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "mod=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "POW_ASSIGN_OP" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "pow=" _ = T_Function [T_String, typeNumber] T_Bool
getTypeFunctionCall "EQ_OP" _ = T_Function [typeAny, typeAny] T_Bool
getTypeFunctionCall "eq" _ = T_Function [typeAny, typeAny] T_Bool
getTypeFunctionCall "NEQ_OP" _ = T_Function [typeAny, typeAny] T_Bool
getTypeFunctionCall "neq" _ = T_Function [typeAny, typeAny] T_Bool
getTypeFunctionCall "LW_OP" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "lw" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "GT_OP" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "gt" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "LWEQ_OP" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "lweq" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "GTEQ_OP" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "gteq" _ = T_Function [typeNumber, typeNumber] T_Bool
getTypeFunctionCall "NOT_OP" _ = T_Function [T_Bool] T_Bool
getTypeFunctionCall "not" _ = T_Function [T_Bool] T_Bool
getTypeFunctionCall "AND_OP" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "and" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "OR_OP" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "or" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "NAND_OP" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "nand" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "NOR_OP" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "nor" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "XOR_OP" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "xor" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "XNOR_OP" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "xnor" _ = T_Function [T_Bool, T_Bool] T_Bool
getTypeFunctionCall "BAND_OP" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "band" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "BOR_OP" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "bor" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "BNOT_OP" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "bnot" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "BXOR_OP" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "bxor" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "LSHIFT_OP" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "lshift" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "RSHIFT_OP" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "rshift" _ = T_Function [typeInteger, typeInteger] typeInteger
getTypeFunctionCall "left" _ = T_Function [T_Tuple (T_Template, T_Template)] T_Template
getTypeFunctionCall "right" _ = T_Function [T_Tuple (T_Template, T_Template)] T_Template
getTypeFunctionCall "swap" _ = T_Function [T_Tuple (T_Template, T_Template)] (T_Tuple (T_Template, T_Template))
getTypeFunctionCall "len" _ = T_Function [T_List T_Template] T_UInt
getTypeFunctionCall "length" _ = T_Function [T_List T_Template] T_UInt
getTypeFunctionCall "concat" _ = T_Function [T_List T_Template, T_List T_Template] (T_List T_Template)
getTypeFunctionCall "split" _ = T_Function [T_List T_Template, T_UInt] (T_Tuple (T_List T_Template, T_List T_Template))
getTypeFunctionCall "first" _ = T_Function [T_List T_Template] T_Template
getTypeFunctionCall "last" _ = T_Function [T_List T_Template] T_Template
getTypeFunctionCall "pushback" _ = T_Function [T_List T_Template, T_Template] (T_List T_Template)
getTypeFunctionCall "pushfront" _ = T_Function [T_List T_Template, T_Template] (T_List T_Template)
getTypeFunctionCall "get" _ = T_Function [T_List T_Template, T_UInt] T_Template
getTypeFunctionCall "reverse" _ = T_Function [T_List T_Template] (T_List T_Template)
getTypeFunctionCall "exp" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "ln" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "max" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "min" _ = T_Function [typeNumber, typeNumber] typeNumber
getTypeFunctionCall "cos" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "acos" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "cosh" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "sin" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "asin" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "sinh" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "tan" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "atan" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "tanh" _ = T_Function [typeNumber] T_Float
getTypeFunctionCall "ceil" _ = T_Function [T_Float] T_Float
getTypeFunctionCall "round" _ = T_Function [T_Float] T_Float
getTypeFunctionCall "trunc" _ = T_Function [T_Float] T_Float
getTypeFunctionCall "floor" _ = T_Function [T_Float] T_Float
getTypeFunctionCall _ [] = T_Undefined
getTypeFunctionCall procedure (ASTDefine name t@(T_Function _ _) _:rest)
    | procedure == name = t
    | otherwise = getTypeFunctionCall procedure rest
getTypeFunctionCall procedure (ASTFunction name parameters _ r:rest)
    | procedure == name = T_Function (map snd parameters) r
    | otherwise = getTypeFunctionCall procedure rest
getTypeFunctionCall procedure (_:rest) = getTypeProcedure procedure rest

getReturnType :: Type -> Type
getReturnType (T_Function _ t) = t
getReturnType _ = T_Undefined

-- AST is the AST expression you want the type of ; [AST] correspond to the output of the convert function in which the function will search for procedure definition
getTypeAST :: AST -> [AST] -> Type
getTypeAST (ASTInt _) _ = T_Int
getTypeAST (ASTUInt _) _ = T_UInt
getTypeAST (ASTChar _) _ = T_Char
getTypeAST (ASTFloat _) _ = T_Float
getTypeAST (ASTBool _) _ = T_Bool
getTypeAST (ASTTuple (a, b)) tt = T_Tuple ((getTypeAST a tt), (getTypeAST b tt))
getTypeAST (ASTString _) _ = T_String
getTypeAST (ASTProcedure procedure) tt = getTypeProcedure procedure tt
getTypeAST (ASTDefine _ _ _) _ = T_Procedure
getTypeAST (ASTFunction _ parameters _ r) _ = T_Function (map snd parameters) r
getTypeAST (ASTLambda parameters _ r) _ = T_Function (map snd parameters) r
getTypeAST (ASTCall (LambdaCall a b c) _) tt = getReturnType $ getTypeAST (ASTLambda a b c) tt
getTypeAST (ASTCall (FunctionCall a) _) tt = getReturnType $ getTypeFunctionCall a tt
getTypeAST (ASTIf _ a b) tt = T_Combination ((getTypeAST a tt):(getTypeAST b tt):[])
getTypeAST (ASTArray list) tt = verifyTypeList $ map (\x -> getTypeAST x tt) list
getTypeAST ASTNULL _ = T_NULL
