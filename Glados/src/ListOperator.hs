module ListOperator (listBuiltins) where

import Data.List (genericSplitAt)
import Data.Maybe (fromMaybe)

import Any
import Bits (word, int)
import DataBuiltins (Symbols, BuiltinsSymbol (BackendBuiltins))
import Utils (Safe(..), mapTuple)

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x : _) 0 = Just x
(!?) (_ : xs) n = (!?) xs (n - 1)

len :: [Any] -> Any
len = UInt . word . length

first :: [Any] -> Any
first [] = NULL
first (x : _) = x

last' :: [Any] -> Any
last' [] = NULL
last' [a] = a
last' (_ : xs) = last' xs

reverse' :: [Any] -> Any
reverse' = Array . reverse

pdpListToValBuiltin :: String -> ([Any] -> Any) -> [Any] -> Safe Any
pdpListToValBuiltin _ f [(Array xs)] = Value (f xs)
pdpListToValBuiltin name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only take one list argument, but got " ++ show (length args) ++ " !")

concat' :: [Any] -> Any -> Safe Any
concat' a (Array b) = Value (Array (a ++ b))
concat' _ b = Error ("concat must be called with two lists, but second argument is " ++ show b ++ " !")

split :: [Any] -> Any -> Safe Any
split xs (UInt n) = Value (Tuple (mapTuple Array (genericSplitAt n xs)))
split _ n = Error ("split index must be an uint, but got " ++ show n ++ " instead !")

pushback :: [Any] -> Any -> Safe Any
pushback xs x = Value (Array (xs ++ [x]))

pushfront :: [Any] -> Any -> Safe Any
pushfront xs x = Value (Array (x : xs))

get :: [Any] -> Any -> Safe Any
get xs (UInt n) = Value (fromMaybe NULL (xs !? (int n)))
get _ n = Error ("get index must be an uint, but got " ++ show n ++ " instead !")

pdpListAndArgToValue :: String -> ([Any] -> Any -> Safe Any) -> [Any] -> Safe Any
pdpListAndArgToValue _ f [(Array xs), a] = f xs a
pdpListAndArgToValue name _ args = Error ("Bad arguments when attempting to call " ++ name ++ ", can only take 2 arguments, but got " ++ show (length args) ++ " !")

listBuiltins :: Symbols
listBuiltins =  [ BackendBuiltins ("len", 1, pdpListToValBuiltin "len" len)
                , BackendBuiltins ("first", 1, pdpListToValBuiltin "first" first)
                , BackendBuiltins ("last", 1, pdpListToValBuiltin "last" last')
                , BackendBuiltins ("reverse", 1, pdpListToValBuiltin "reverse" reverse')
                , BackendBuiltins ("concat", 2, pdpListAndArgToValue "concat" concat')
                , BackendBuiltins ("split", 2, pdpListAndArgToValue "split" split)
                , BackendBuiltins ("pushback", 2, pdpListAndArgToValue "pushback" pushback)
                , BackendBuiltins ("pushfront", 2, pdpListAndArgToValue "pushfront" pushfront)
                , BackendBuiltins ("get", 2, pdpListAndArgToValue "get" get) ]