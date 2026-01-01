module ASTVerification (verifAST) where

import Utils
import Type
import AST

verifArgAST :: AST -> Type -> [AST] -> Bool
verifArgAST ast@(ASTCall _ _) parameter tt = (verifyType parameter $ getTypeAST ast tt) && (verifParamAST ast tt)
verifArgAST ast@(ASTIf _ a b) parameter tt = (verifyType parameter $ getTypeAST a tt) && (verifyType parameter $ getTypeAST b tt) && (verifParamAST ast tt)
verifArgAST ast parameter tt = verifyType parameter $ getTypeAST ast tt

verifBody :: [Parameter] -> AST -> Type -> [AST] -> Bool
verifBody params body ret tt = verifArgAST body ret (tt ++ (map (\p -> case p of
    (ASTProcedure n, t) -> ASTDefine n t ASTNULL
    _ -> ASTNULL) params)) -- default value for a case that must never happend but pattern matching isn't smart enough to know it

funcTypeToLambdaCall :: Type -> Call
funcTypeToLambdaCall (T_Function params ret) = LambdaCall (map (\x -> (ASTProcedure "a", x)) params) ASTNULL ret
funcTypeToLambdaCall _ = LambdaCall [] ASTNULL T_Int -- default value for a case that must never happend but pattern matching isn't smart enough to know it

verifParamAST :: AST -> [AST] -> Bool
verifParamAST (ASTInt _) _ = True
verifParamAST (ASTUInt _) _ = True
verifParamAST (ASTChar _) _ = True
verifParamAST (ASTFloat _) _ = True
verifParamAST (ASTBool _) _ = True
verifParamAST (ASTTuple _) _ = True
verifParamAST (ASTArray list) tt = (verifyTypeList $ map (\x -> getTypeAST x tt) list) /= T_Undefined
verifParamAST (ASTString _) _ = True
verifParamAST (ASTProcedure _) _ = True
verifParamAST (ASTDefine _ t ast) tt = verifArgAST ast t tt
verifParamAST (ASTFunction _ params body ret) tt = verifBody params body ret tt
verifParamAST (ASTLambda params body ret) tt = verifBody params body ret tt
verifParamAST (ASTCall (LambdaCall params body ret) args) tt
    | (length params) == (length args) = (all (== True) $ map (\(p, a) -> verifArgAST a p tt) $ zip (map snd params) args) && (verifBody params body ret tt)
    | otherwise = False
verifParamAST (ASTCall (FunctionCall name) args) tt = verifParamAST (ASTCall (funcTypeToLambdaCall $ getTypeFunctionCall name tt) args) tt
verifParamAST (ASTIf ast _ _) tt = verifArgAST ast T_Bool tt
verifParamAST ASTNULL _ = True

verifAST' :: [AST] -> [AST] -> Bool
verifAST' [] _ = True
verifAST' [branch] total = verifParamAST branch total
verifAST' (branch:rest) total = (verifParamAST branch total) && (verifAST' rest total)

verifAST :: Safe [AST] -> Safe [AST]
verifAST (Error err) = Error err
verifAST (Value ast)
    | verifAST' ast ast = Value ast
    | otherwise = Error "GLaDOS: TypeError: Incompatible types."
