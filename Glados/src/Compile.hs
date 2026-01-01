{-# LANGUAGE TupleSections #-}

module Compile (compileAST) where

import Control.Applicative (liftA3, (<|>))
import Data.Functor ((<&>))
import Data.List (singleton, elemIndex)
import Data.Typeable (Typeable)
import Data.Word (Word8)

import Any (Any(..), makeAny)
import AssemblyInstructions (AssemblyInstruction(..), assemble, toAssemblyValueInstruction, astToAny, RegisterID, OutputAssemblyInstruction(..))
import AST (AST(..), Call(..), getTypeAST, Parameter)
import Bits (u32)
import SymbolTable (SymbolTable, writeSymbolTable)
import Type
import Utils (Safe(..), maybeToSafe, alternativeMap, bind2, concatMapM, tupleConcat)
import VMData (Address)

data CompilationStatus = CompilationStatus {
    _instructions :: [AssemblyInstruction],
    _symbols :: [(String, CompilationStatus)],
    _params :: [[Parameter]]
}

emptyCompilationStatus :: CompilationStatus
emptyCompilationStatus = CompilationStatus {
    _instructions = [],
    _symbols = [],
    _params = []
}

statusFromInstructions :: [AssemblyInstruction] -> CompilationStatus
statusFromInstructions instructions = CompilationStatus {
    _instructions = instructions,
    _symbols = [],
    _params = []
}

(+++) :: CompilationStatus -> CompilationStatus -> Safe CompilationStatus
(+++) comp1 comp2
    | any (`elem` (map fst (_symbols comp2))) (map fst (_symbols comp1)) = Error "Duplicate symbols !"
    | otherwise = Value CompilationStatus {
        _instructions = _instructions comp1 ++ _instructions comp2,
        _symbols = _symbols comp1 ++ _symbols comp2,
        _params = _params comp2 ++ _params comp1
    }

addSymbol :: CompilationStatus -> String -> CompilationStatus -> Safe CompilationStatus
addSymbol status name status'
    | any ((== name) . fst) (_symbols status) = Error ("Symbol " ++ name ++ " already exists !")
    | otherwise = Value status {
        _symbols = (_symbols status) ++ [(name, status')]
    }

compileValue :: (Show a, Typeable a) => Type -> a -> Bool -> Safe CompilationStatus
compileValue _type value isNested = makeAny _type value <&> (\val -> compileValueFromAny val isNested)

compileValueFromAny :: Any -> Bool -> CompilationStatus
compileValueFromAny val isNested = CompilationStatus {
    _instructions = [(if isNested then PushValue else OutValue) val],
    _symbols = [],
    _params = []
}

popAllArgs :: Int -> [AssemblyInstruction]
popAllArgs n = map Pop [0..((fromIntegral n) - 1)]

compileCall :: String -> [AST] -> Bool -> CompilationStatus -> Safe CompilationStatus
compileCall symbol args isNested status = concatMapM compileArg (reverse args) <&> (statusFromInstructions . (++ outIfNotNested) . (++ [Call symbol]))
    where outIfNotNested = if isNested then [] else [Pop 0, OutRegister 0]
          compileArg :: AST -> Safe [AssemblyInstruction]
          compileArg arg = case arg of
            ASTCall (FunctionCall symbol') args' -> compileCall symbol' args' True status <&> _instructions
            ASTProcedure name -> (paramIndex name <&> (\i -> [PushRegister i])) <|> Value [Call name]
            _ -> singleton <$> toAssemblyValueInstruction PushValue arg
            where paramIndex name' = findParamIndex name' status

compileElem :: AST -> Safe [AssemblyInstruction]
compileElem a = case a of
    (ASTCall (FunctionCall name) args) -> mapM (\a' -> astToAny a' <&> PushValue) (reverse args) <&> (++ [Call name])
    _ -> astToAny a <&> singleton . PushValue

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = null (filter (/= x) xs)

haveSameType :: [AST] -> Safe Bool
haveSameType list = mapM (\x -> Value $ getTypeAST x []) list <&> allEqual

findParamIndex :: String -> CompilationStatus -> Safe RegisterID
findParamIndex name (CompilationStatus _ _ params) = maybeToSafe (show name ++ " isn't a function parameter !") foundIndex
    where
          findParamIndex1 :: String -> [Parameter] -> Maybe RegisterID
          findParamIndex1 name' params' = elemIndex (ASTProcedure name') (map fst params') <&> fromIntegral

          foundIndex = foldr (\params' index -> index <|> findParamIndex1 name params') Nothing params

pushParams :: [Parameter] -> CompilationStatus -> CompilationStatus
pushParams params status = status {
    _params = params : _params status
}

popParams :: CompilationStatus -> CompilationStatus
popParams status = status {
    _params = drop 1 (_params status) -- tail is partial, thus causes a fatal error if the list is empty, but drop does not.
}

compileFunction :: AST -> [Parameter] -> CompilationStatus -> Bool -> Safe CompilationStatus
compileFunction ast params status isNested = compileAST1 statusWithParams ast isNested <&> popParams >>= ((statusFromInstructions (popAllArgs (length params))) +++)
    where statusWithParams = pushParams params status

compileAST1 :: CompilationStatus -> AST -> Bool -> Safe CompilationStatus
compileAST1 status (ASTChar c) isNested = compileValue T_Char c isNested >>= (status +++)
compileAST1 status (ASTInt n) isNested = compileValue T_Int n isNested >>= (status +++)
compileAST1 status (ASTUInt n) isNested = compileValue T_UInt n isNested >>= (status +++)
compileAST1 status (ASTFloat n) isNested = compileValue T_Float n isNested >>= (status +++)
compileAST1 status (ASTBool b) isNested = compileValue T_Bool b isNested >>= (status +++)
compileAST1 status (ASTString str) isNested = compileValue T_String str isNested >>= (status +++)
compileAST1 status (ASTCall (FunctionCall f) args) isNested = compileCall f args isNested status >>= (status +++)
compileAST1 _ (ASTDefine s _ _) True = Error ("Error when trying to define procedure " ++ s ++ ": nested procedures are forbidden !")
compileAST1 status (ASTDefine s _type ast) False = compileAST1 emptyCompilationStatus ast True >>= (+++ statusFromInstructions [Ret]) >>= addSymbol status s
compileAST1 status (ASTArray []) isNested = compileValue T_EmptyList ([] :: [Int]) isNested >>= (status +++)
compileAST1 status (ASTProcedure name) _ = status +++ (statusFromInstructions $ singleton $ alternativeMap (PushRegister) (Call name) index)
    where index = findParamIndex name status

compileAST1 status astList@(ASTArray list) isNested = Value (getTypeAST astList []) >>= (\type' -> concatMapM compileElem (reverse list) <&> (++ [Construct type' (length list)] ++ outputIfNotNested) >>= ((status +++) . statusFromInstructions))
    where outputIfNotNested = if isNested then [] else [Pop 0, OutRegister 0]

compileAST1 status astTuple@(ASTTuple (a, b)) isNested = Value (getTypeAST astTuple []) >>= (\type' -> liftA2 (\a' b' -> b' ++ a' ++ [Construct type' 2] ++ outputIfNotNested) (compileElem a) (compileElem b)) >>= ((status +++) . statusFromInstructions)
    where outputIfNotNested = if isNested then [] else [Pop 0, OutRegister 0]

compileAST1 status (ASTIf condition trueValue falseValue) isNested = haveBothValuesTheSameType >> concatInstructions conditionCompiled trueValueCompiled falseValueCompiled >>= ((status +++) . statusFromInstructions)
    where haveBothValuesTheSameType = haveSameType [trueValue, falseValue]
          conditionCompiled = compileAST1 status condition True <&> _instructions <&> (++ [Test])
          trueValueCompiled = compileAST1 status trueValue isNested <&> _instructions
          falseValueCompiled = compileAST1 status falseValue isNested <&> _instructions
          concatInstructions = liftA3 (\conditionCode trueCode falseCode -> [If conditionCode trueCode falseCode])

compileAST1 status (ASTLambda params ast returnType) isNested = if isNested then compileFunction ast params status isNested >>= (status +++) else makeAny (T_Function (map snd params) returnType) (0 :: Int) >>= (\lambdaType -> status +++ (statusFromInstructions [OutValue lambdaType])) -- don't execute lambda if not used
compileAST1 status (ASTCall (LambdaCall params ast _) args) isNested = bind2 (\args' code -> statusFromInstructions args' +++ code) pushArgs functionCode >>= (status +++)
    where checkArgs = if (length args) > 16 then Error "Too many arguments (16 max) !" else Value args
          paramName :: Parameter -> Safe String
          paramName (ASTProcedure name, _) = Value name
          paramName (ast', _) = Error ("Illegal parameter " ++ show ast' ++ ", must be ASTProcedure ... !")

          argsToAny = liftA2 (\args' paramNames -> reverse $ zip paramNames (map astToAny args')) checkArgs (mapM paramName params)
          pushArgs = argsToAny <&> (map (\(name, any') -> alternativeMap PushValue (Call name) any'))
          functionCode = compileFunction ast params status isNested

compileAST1 status (ASTFunction name params ast _) isNested = nestedProcedureError >> compileFunction ast params status True >>= (+++ statusFromInstructions [Ret]) >>= addSymbol status name
    where nestedProcedureError = if isNested then Error ("Error when trying to define procedure " ++ name ++ ": nested procedures are forbidden !") else Value (0 :: Int)

compileAST1 _ a _ = Error ("Compiling " ++ show a ++ " isn't not implemented for now !")

compileAST' :: CompilationStatus -> [AST] -> Safe CompilationStatus
compileAST' status [] = Value status
compileAST' status (x : xs)
    | null xs = compiled
    | otherwise = compiled >>= (\status' -> compileAST' status' xs)
    where compiled = compileAST1 status x False

makeSymbolTable' :: Address -> [(String, CompilationStatus)] -> Safe (SymbolTable, [OutputAssemblyInstruction], [Word8])
makeSymbolTable' offset [] = Value ([("__main", offset)], [Label "__main"], [])
makeSymbolTable' offset ((name, CompilationStatus instructions _ _) : xs) = assemble instructions >>= (\instructions' -> next instructions' <&> (tupleConcat ([(name, offset)], (Label name) : map OutputAssemblyInstruction instructions, instructions')))
    where next instructions'' = makeSymbolTable' (offset + u32 (length instructions'')) xs

makeSymbolTable :: [(String, CompilationStatus)] -> Safe (SymbolTable, [OutputAssemblyInstruction], [Word8])
makeSymbolTable = makeSymbolTable' 0

finishCompilation :: CompilationStatus -> Safe ([OutputAssemblyInstruction], [Word8])
finishCompilation (CompilationStatus instructions symbols _) = liftA2 (\(symtab', symbolsInstructions, symbols') instructions' -> (symbolsInstructions ++ (map OutputAssemblyInstruction instructions), writeSymbolTable symtab' ++ symbols' ++ instructions')) symtab (assemble instructions)
    where symtab = makeSymbolTable symbols

compileAST :: [AST] -> Safe ([OutputAssemblyInstruction], [Word8])
compileAST ast = compileAST' emptyCompilationStatus ast >>= finishCompilation
