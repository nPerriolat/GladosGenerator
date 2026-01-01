module Scanner (
    AlmostSExpr(ASExpr, SListBegin, SListEnd),
    convertToASExpr,
    stringToASExpr,
    scanParanthese,
    fromSafeSExpr,
    concatSafe,
    verifyParanthese,
    aSExprToSExpr,
    scan
    ) where

import Text.Read
import Data.Maybe
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isSpace, chr)

import SExpression
import Utils

import Base

data AlmostSExpr = ASExpr SExpr | SListBegin | SListEnd | STupleBegin | STupleEnd | SArrayBegin | SArrayEnd | SFunctionTypeBegin | SFunctionTypeEnd deriving (Eq, Show)

-- take any string and return a SNumber if the string is an integer, return a SSymbol otherwise
convertToASExpr :: String -> Safe [AlmostSExpr]
convertToASExpr [] = Value []
convertToASExpr str@(_:xs)
    | isPrefixOf "EXPRESSION_LEFT_DELIMITER" str = case convertToASExpr xs of
                                Value result -> Value (SListBegin : result)
                                Error err -> Error err

    | isPrefixOf "EXPRESSION_RIGHT_DELIMITER" (reverse str) = case convertToASExpr (reverse (drop 1 (reverse str))) of
                                        Value result -> Value (result ++ [SListEnd])
                                        Error err -> Error err

    | isPrefixOf "TUPLE_LEFT_DELIMITER" str = case convertToASExpr xs of
                                Value result -> Value (STupleBegin : result)
                                Error err -> Error err

    | isPrefixOf "TUPLE_RIGHT_DELIMITER" (reverse str) = case convertToASExpr (reverse (drop 1 (reverse str))) of
                                        Value result -> Value (result ++ [STupleEnd])
                                        Error err -> Error err

    | isPrefixOf "ARRAY_LEFT_DELIMITER" str = case convertToASExpr xs of
                                Value result -> Value (SArrayBegin : result)
                                Error err -> Error err

    | isPrefixOf "ARRAY_RIGHT_DELIMITER" (reverse str) = case convertToASExpr (reverse (drop 1 (reverse str))) of
                                        Value result -> Value (result ++ [SArrayEnd])
                                        Error err -> Error err

    | isPrefixOf "LAMBDA_TYPE_LEFT_DELIMITER" str = case convertToASExpr (drop 2 str) of
                                Value result -> Value (SFunctionTypeBegin : result)
                                Error err -> Error err

    | isPrefixOf (reverse "LAMBDA_TYPE_RIGHT_DELIMITER") (reverse str) && not (isPrefixOf "DEFINE_OP" str) =
        case convertToASExpr (reverse (drop 2 (reverse str))) of
            Value result -> Value (result ++ [SFunctionTypeEnd])
            Error err -> Error err

    | head str == '"' && last str == '"' =
        case length str of
            2 -> Error "SyntaxError: litteral can't be empty"
            _ -> Value [ASExpr (SString (tail (init str)))]

    | head str == '\'' && last str == '\'' =
        case length str of
            3 -> Value [ASExpr (SChar (str !! 1))]
            2 -> Error "SyntaxError: litteral can't be empty"
            _ -> Error "SyntaxError: string is not a char"

    | isValidInt str =
        case readMaybe str of
            Just f -> Value [ASExpr (SNumber f)]
            Nothing -> Error "error while converting to float"

    | isValidFloat str =
        case readMaybe str of
            Just f -> Value [ASExpr (SFloat f)]
            Nothing -> Error "error while converting to float"

    | isSuffixOf "i" str && isValidInt (init str) =
        case readMaybe (init str) of
            Just i -> Value [ASExpr (SNumber i)]
            Nothing -> Error "error while converting to int"

    | isSuffixOf "u" str && isValidInt (init str) =
        case readMaybe (init str) of
            Just i ->
                if i >= 0
                    then
                        Value [ASExpr (SUint i)]
                else
                    Error "SyntaxError: invalid unsigned int"
            Nothing -> Error "error while converting to int"

    | isSuffixOf "c" str && isValidInt (init str) = 
        case readMaybe (init str) of
            Just i ->
                (if (i < 0) || (i > 255) then
                    Error "SyntaxError: char are only between 0 and 255"
                else
                    Value [ASExpr (SChar (chr i))])
            Nothing -> Error "error while converting to int"

    | isNothing (readMaybe str :: Maybe Int) =
        case processToken str of
            Value result -> 
                if result == str
                    then Error ("SyntaxError: Invalid base number: (" ++ str ++ ")")
                    else case readMaybe result of
                            Just n  -> Value [ASExpr (SNumber n)]
                            Nothing -> Error "SyntaxError: Invalid decimal number format"
            Error "Basic String" -> Value [ASExpr (SSymbol str)]
            Error _ -> Value [ASExpr (SSymbol str)]

    | otherwise = case readMaybe str of
                    Just n  -> Value [ASExpr (SNumber n)]
                    Nothing -> Error "Invalid number format"

isValidInt :: String -> Bool
isValidInt str = case readMaybe str :: Maybe Int of
                    Just _  -> True
                    Nothing -> False

isValidFloat :: String -> Bool
isValidFloat str = case readMaybe str :: Maybe Float of
                    Just _  -> True
                    Nothing -> False

stringToASExpr :: [String] -> [AlmostSExpr] -> Safe [AlmostSExpr]
stringToASExpr [] result = Value result
stringToASExpr (x:xs) result = case convertToASExpr x of
    Value newExpr -> stringToASExpr xs (result ++ newExpr)
    Error err -> Error err

-- list [] 0
-- (le reste de la liste, la liste des paranthèses)
scanParanthese :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
scanParanthese [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ')' expected\n"
scanParanthese (SListEnd:rList) pList 0 = Value (rList, reverse pList)
scanParanthese (SListEnd:rList) pList i = scanParanthese rList (SListEnd:pList) (i - 1)
scanParanthese (SListBegin:rList) pList i = scanParanthese rList (SListBegin:pList) (i + 1)
scanParanthese (r:rList) pList i = scanParanthese rList (r:pList) i

-- Parses a tuple structure.
scanTuple :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
scanTuple [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, '}' expected\n"
scanTuple (STupleEnd:rList) pList 0 = Value (rList, reverse pList)
scanTuple (STupleEnd:rList) pList i = scanTuple rList (STupleEnd:pList) (i - 1)
scanTuple (STupleBegin:rList) pList i = scanTuple rList (STupleBegin:pList) (i + 1)
scanTuple (r:rList) pList i = scanTuple rList (r:pList) i

-- Parses an array structure.
scanArray :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
scanArray [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, ']' expected\n"
scanArray (SArrayEnd:rList) pList 0 = Value (rList, reverse pList)
scanArray (SArrayEnd:rList) pList i = scanArray rList (SArrayEnd:pList) (i - 1)
scanArray (SArrayBegin:rList) pList i = scanArray rList (SArrayBegin:pList) (i + 1)
scanArray (r:rList) pList i = scanArray rList (r:pList) i

scanFunctionType :: [AlmostSExpr] -> [AlmostSExpr] -> Int -> Safe ([AlmostSExpr], [AlmostSExpr])
scanFunctionType [] _ _ = Error "GLaDOS: SyntaxError: unexpected EOF while parsing, '>' expected\n"
scanFunctionType (SFunctionTypeEnd:rList) pList 0 = Value (rList, reverse pList)
scanFunctionType (SFunctionTypeEnd:rList) pList i = scanFunctionType rList (SFunctionTypeEnd:pList) (i - 1)
scanFunctionType (SFunctionTypeBegin:rList) pList i = scanFunctionType rList (SFunctionTypeBegin:pList) (i + 1)
scanFunctionType (r:rList) pList i = scanFunctionType rList (r:pList) i

fromSafeSExpr :: Safe [SExpr] -> Safe SExpr
fromSafeSExpr (Value list) = Value (SList list)
fromSafeSExpr (Error err) = Error err

fromSafeTuple :: Safe [SExpr] -> Safe SExpr
fromSafeTuple (Value list) = Value (STuple list)
fromSafeTuple (Error err) = Error err

fromSafeArray :: Safe [SExpr] -> Safe SExpr
fromSafeArray (Value list) = Value (SArray list)
fromSafeArray (Error err) = Error err

fromSafeFunctionType :: Safe [SExpr] -> Safe SExpr
fromSafeFunctionType (Value list) = Value (SFunctionType list)
fromSafeFunctionType (Error err) = Error err

concatSafe :: Safe SExpr -> Safe [SExpr] -> Safe [SExpr]
concatSafe (Value e) (Value es) = Value (e:es)
concatSafe (Error err1) (Error err2) = Error ("2 Errors encountered at the same time: " ++ err1 ++ " ; " ++ err2)
concatSafe (Error err) _ = Error err
concatSafe _ (Error err) = Error err

verifyParanthese :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyParanthese (Value (rList, pList)) list = aSExprToSExpr rList (concatSafe (fromSafeSExpr (aSExprToSExpr pList (Value []))) list)
verifyParanthese (Error err) _ = Error err

-- return a list of AlmostSexpr trunc to the correct SListEnd (and trunc it)
toSListEnd :: [AlmostSExpr] -> Int  -> [AlmostSExpr]
toSListEnd [] _ = []
toSListEnd (x:xs) nbStructure
    | nbStructure == 0 =
        case x of
            SListEnd -> xs
            SListBegin -> toSListEnd xs (nbStructure + 1)
            _ -> toSListEnd xs nbStructure
    | otherwise =
        case x of
            SListEnd -> toSListEnd xs (nbStructure - 1)
            SListBegin -> toSListEnd xs (nbStructure + 1)
            _ -> toSListEnd xs nbStructure

-- return a list of AlmostSexpr trunc to the first STupleEnd (and trunc it)
toSTupleEnd :: [AlmostSExpr] -> Int  -> [AlmostSExpr]
toSTupleEnd [] _ = []
toSTupleEnd (x:xs) nbStructure
    | nbStructure == 0 =
        case x of
            STupleEnd -> xs
            STupleBegin -> toSTupleEnd xs (nbStructure + 1)
            _ -> toSTupleEnd xs nbStructure
    | otherwise =
        case x of
            STupleEnd -> toSTupleEnd xs (nbStructure - 1)
            STupleBegin -> toSTupleEnd xs (nbStructure + 1)
            _ -> toSTupleEnd xs nbStructure

-- return a list of AlmostSexpr trunc to the first STupleEnd (and trunc it)
toSArrayEnd :: [AlmostSExpr] -> Int -> [AlmostSExpr]
toSArrayEnd [] _ = []
toSArrayEnd (x:xs) nbStructure
    | nbStructure == 0 =
        case x of
            SArrayEnd -> xs
            SArrayBegin -> toSArrayEnd xs (nbStructure + 1)
            _ -> toSArrayEnd xs nbStructure
    | otherwise =
        case x of
            SArrayEnd -> toSArrayEnd xs (nbStructure - 1)
            SArrayBegin -> toSArrayEnd xs (nbStructure + 1)
            _ -> toSArrayEnd xs nbStructure

-- return a list of AlmostSexpr trunc to the first SFunctionTypeEnd (and trunc it)
toSFunctionTypeEnd :: [AlmostSExpr] -> Int -> [AlmostSExpr]
toSFunctionTypeEnd [] _ = []
toSFunctionTypeEnd (x:xs) nbStructure
    | nbStructure == 0 =
        case x of
            SFunctionTypeEnd -> xs
            SFunctionTypeBegin -> toSFunctionTypeEnd xs (nbStructure + 1)
            _ -> toSFunctionTypeEnd xs nbStructure
    | otherwise =
        case x of
            SFunctionTypeEnd -> toSFunctionTypeEnd xs (nbStructure - 1)
            SFunctionTypeBegin -> toSFunctionTypeEnd xs (nbStructure + 1)
            _ -> toSFunctionTypeEnd xs nbStructure

-- bollean for checkValidTuple
isValidTuple :: Int -> [AlmostSExpr] -> Bool
isValidTuple 1 [] = False
isValidTuple _ [] = True
isValidTuple nbElement (x:xs)
    | nbElement > 2 = False
    | otherwise =
        case x of
            SListBegin -> isValidTuple nbElement (toSListEnd xs 0)
            STupleBegin -> isValidTuple nbElement (toSTupleEnd xs 0)
            SArrayBegin -> isValidTuple nbElement (toSArrayEnd xs 0)
            SFunctionTypeBegin -> isValidTuple nbElement (toSFunctionTypeEnd xs 0)
            ASExpr (SSymbol ",") -> isValidTuple (nbElement + 1) xs
            _ -> isValidTuple nbElement xs

-- return a Tuple if is a Valid, else return an Err
checkValidTuple :: [AlmostSExpr] -> Safe [AlmostSExpr]
checkValidTuple list
    | isValidTuple 1 list = Value list
    | otherwise = Error "GLaDOS: SyntaxError: Invalid tuple detected"

verifyTuple :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyTuple (Value (rList, pList)) list =
    let tuple = checkValidTuple pList
    in case tuple of
        Value validTuple ->
            aSExprToSExpr rList (concatSafe (fromSafeTuple (aSExprToSExpr validTuple (Value []))) list)
        Error err ->
            Error err
verifyTuple (Error err) _ = Error err

verifyArray :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyArray (Value (rList, pList)) list = 
    let processedList = aSExprToSExpr rList (concatSafe (fromSafeArray (aSExprToSExpr pList (Value []))) list)
    in case processedList of
        Error err -> Error err
        Value exprs -> Value exprs
verifyArray (Error err) _ = Error err

verifyFunctionType :: Safe ([AlmostSExpr], [AlmostSExpr]) -> Safe [SExpr] -> Safe [SExpr]
verifyFunctionType (Value (rList, pList)) list =
    aSExprToSExpr rList (concatSafe (fromSafeFunctionType (aSExprToSExpr pList (Value []))) list)
verifyFunctionType (Error err) _ = Error err

aSExprToSExpr :: [AlmostSExpr] -> Safe [SExpr] -> Safe [SExpr]
aSExprToSExpr _ (Error err) = Error err
aSExprToSExpr [] (Value list) = Value (reverse list)
aSExprToSExpr ((ASExpr expr):xs) (Value list) = aSExprToSExpr xs (Value (expr:list))
aSExprToSExpr (SListEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected ')' while parsing\n"
aSExprToSExpr (SListBegin:xs) list = verifyParanthese (scanParanthese xs [] 0) list
aSExprToSExpr (STupleBegin:xs) list = verifyTuple (scanTuple xs [] 0) list
aSExprToSExpr (STupleEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected '}' while parsing\n"
aSExprToSExpr (SArrayBegin:xs) list = verifyArray (scanArray xs [] 0) list
aSExprToSExpr (SArrayEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected ']' while parsing\n"
aSExprToSExpr (SFunctionTypeBegin:xs) list = verifyFunctionType (scanFunctionType xs [] 0) list
aSExprToSExpr (SFunctionTypeEnd:_) _ = Error "GLaDOS: SyntaxError: unexpected '>' while parsing\n"

-- recursive function for check if array, tuple, etc.. are not interlocked
verifyASExpr :: Maybe Char -> Int -> [AlmostSExpr] -> Safe (Int, [AlmostSExpr])
verifyASExpr char index list
    | index > length list - 1 = Value (index, list)
    | otherwise =
        if charNothing
            then
                let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr Nothing (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr Nothing (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr Nothing (returnIndex + 1) list
                            Error err -> Error err
                    SFunctionTypeBegin ->
                        let result = verifyASExpr (Just '<') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr Nothing (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Error "SyntaxError: Unexpecting closing paranthese found"
                    STupleEnd -> Error "SyntaxError: Unexpecting closing curly bracket found"
                    SArrayEnd -> Error "SyntaxError: Unexpecting closing bracket found"
                    SFunctionTypeEnd -> Error "SyntaxError: Unexpected closing function type bracket found"
                    _ ->  verifyASExpr Nothing (index + 1) list
        else if charParanthese
            then
                let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SFunctionTypeBegin ->
                        let result = verifyASExpr (Just '<') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Value (index, list)
                    STupleEnd -> Error "SyntaxError: Interlocked Tuple in ()"
                    SArrayEnd -> Error "SyntaxError: Interlocked Array in ()"
                    SFunctionTypeEnd -> Error "SyntaxError: Interlocked FunctionType in ()"
                    _ -> verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER')  (index + 1) list
        else if charCurlyBrack
            then
                let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SFunctionTypeBegin ->
                        let result = verifyASExpr (Just '<') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Error "SyntaxError: Interlocked () in Tuple"
                    STupleEnd -> Value (index, list)
                    SArrayEnd -> Error "SyntaxError: Interlocked Array in Tuple"
                    SFunctionTypeEnd -> Error "SyntaxError: Interlocked FunctionType in Tuple"
                    _ -> verifyASExpr (Just 'TUPLE_LEFT_DELIMITER')  (index + 1) list
        else if charBrack
            then
            let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SFunctionTypeBegin ->
                        let result = verifyASExpr (Just '<') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Error "SyntaxError: Interlocked () in Array"
                    STupleEnd -> Error "SyntaxError: Interlocked Tuple in Array"
                    SArrayEnd -> Value (index, list)
                    SFunctionTypeEnd -> Error "SyntaxError: Interlocked FunctionType in Array"
                    _ -> verifyASExpr (Just 'ARRAY_LEFT_DELIMITER')  (index + 1) list
        else if charFunctionType
            then
                let currentElement = list !! index
                in case currentElement of
                    SListBegin ->
                        let result = verifyASExpr (Just 'EXPRESSION_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '<') (returnIndex + 1) list
                            Error err -> Error err
                    STupleBegin ->
                        let result = verifyASExpr (Just 'TUPLE_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '<') (returnIndex + 1) list
                            Error err -> Error err
                    SArrayBegin ->
                        let result = verifyASExpr (Just 'ARRAY_LEFT_DELIMITER') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '<') (returnIndex + 1) list
                            Error err -> Error err
                    SFunctionTypeBegin ->
                        let result = verifyASExpr (Just '<') (index + 1) list
                        in case result of
                            Value (returnIndex, _) -> verifyASExpr (Just '<') (returnIndex + 1) list
                            Error err -> Error err
                    SListEnd -> Error "SyntaxError: Interlocked () in FunctionType"
                    STupleEnd -> Error "SyntaxError: Interlocked Tuple in FunctionType"
                    SArrayEnd -> Error "SyntaxError: Interlocked Array in FunctionType"
                    SFunctionTypeEnd -> Value (index, list)
                    _ -> verifyASExpr (Just '<')  (index + 1) list
        else
            error "You unlock an achivement because this is impossible. Unexpected character passes as argument"
    where
        charNothing = isNothing char
        charParanthese = char == Just 'EXPRESSION_LEFT_DELIMITER'
        charCurlyBrack = char == Just 'TUPLE_LEFT_DELIMITER'
        charBrack = char == Just 'ARRAY_LEFT_DELIMITER'
        charFunctionType = char == Just '<'

customWords :: String -> [String]
customWords [] = []
customWords ('"':xs) =
    let (quoted, rest) = span (/= '"') xs
    in ['"' : quoted ++ "\""] ++ customWords (drop 1 rest)
customWords ('\'':xs) =
    let (quoted, rest) = span (/= '\'') xs
    in ['\'' : quoted ++ "'"] ++ customWords (drop 1 rest)
customWords (x:xs)
    | x == ','  = [","] ++ customWords xs
    | isSpace x = customWords xs
    | otherwise = 
        let (word, rest) = break (\c -> isSpace c || c == ',') (x:xs)
        in [word] ++ customWords rest

removeCommas :: Safe [SExpr] -> Safe [SExpr]
removeCommas (Error err) = Error err
removeCommas (Value list) = Value (map removeCommasFromExpr list)
    where
    removeCommasFromExpr :: SExpr -> SExpr
    removeCommasFromExpr (SList exprs) = SList (map removeCommasFromExpr (filter (not . isComma) exprs))
    removeCommasFromExpr (STuple exprs) = STuple (map removeCommasFromExpr (filter (not . isComma) exprs))
    removeCommasFromExpr (SArray exprs) = SArray (map removeCommasFromExpr (filter (not . isComma) exprs))
    removeCommasFromExpr (SFunctionType exprs) = SFunctionType (map removeCommasFromExpr (filter (not . isComma) exprs))
    removeCommasFromExpr expr = expr  -- Leave other expressions unchanged

    isComma :: SExpr -> Bool
    isComma (SSymbol ",") = True
    isComma _ = False

scan :: String -> Safe [SExpr]
scan str =
    let result = stringToASExpr (customWords str) []
    in case result of
        Value asExprList ->
            case verifyASExpr Nothing 0 asExprList of
                Value (_, list) ->
                    removeCommas (aSExprToSExpr list (Value []))
                Error err -> Error err
        Error err -> Error err