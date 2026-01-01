{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Any (
    Any(..),
    AnyAssembly(..),
    AnyVM(..),
    makeAny,
    anyType,
    (~<=),
    (~>=),
    (~>),
    (~<),
    min',
    max',
    (~+),
    (~-),
    (~*),
    (~/),
    negate',
    abs',
    (~==),
    (~/=)
) where

import Control.Applicative (liftA3, (<|>))
import Data.ByteString.Internal (c2w, w2c)
import Data.Functor ((<&>))
import qualified Data.Kind (Type)
import Data.List (intersperse)
import Data.Proxy
import Data.Typeable
import Data.Word (Word8)
import Foreign.Marshal.Utils (fromBool)
import GHC.Float (int2Float, float2Int, floorFloat)

import Bits (word, i32, int)
import Hex (showHex8)
import Serialize (Serializable(..), serializeTypeNull, serializeTypeEmptyList, Func(..))
import Type (Type(..))
import Utils (Safe(..), safeCast)

data Any =  Int Int                         |
            UInt Word                       |
            Char Char                       |
            Float Float                     |
            Bool Bool                       |
            String String                   |
            EmptyArray                      |
            Array [Any]                     |
            Tuple (Any, Any)                |
            UncalledFunction [Type] Type    |
            NULL                            deriving (Eq, Show, Typeable)

data AnyAssembly = AnyAssembly Any

instance Show AnyAssembly where
    show :: AnyAssembly -> String
    show (AnyAssembly (Int n)) = "int " ++ show n
    show (AnyAssembly (UInt n)) = "uint " ++ show n
    show (AnyAssembly (Char c)) = "char " ++ showHex8 (c2w c) ++ " (" ++ show c ++ ")"
    show (AnyAssembly (Float f)) = "float " ++ show f
    show (AnyAssembly (Bool b)) = "bool " ++ show b
    show (AnyAssembly (String str)) = "string " ++ show str
    show (AnyAssembly EmptyArray) = "[]"
    show (AnyAssembly (Array xs)) = "[" ++ (concat (intersperse ", " (map show xs))) ++ "]"
    show (AnyAssembly (Tuple (a, b))) = "{" ++ show a ++ ", " ++ show b ++ "}"
    show (AnyAssembly (UncalledFunction types returnType)) = "<-(" ++ concat (intersperse " " (map show types)) ++ ") => " ++ show returnType ++ "->"
    show (AnyAssembly NULL) = "NULL"

data AnyVM = AnyVM Any

instance Show AnyVM where
    show :: AnyVM -> String
    show (AnyVM (Int n)) = show n
    show (AnyVM (UInt n)) = show n
    show (AnyVM (Char c)) = show c
    show (AnyVM (Float f)) = show f
    show (AnyVM (Bool b)) = if b then "#t" else "#f"
    show (AnyVM (String str)) = show str
    show (AnyVM EmptyArray) = "[]"
    show (AnyVM (Array xs@((Char _) : _))) = "\"" ++ (concatMap toChar xs) ++ "\""
        where toChar :: Any -> String
              toChar (Char c) = [c]
              toChar _ = ""
    show (AnyVM (Array xs)) = "[" ++ (concat (intersperse ", " (map show (map AnyVM xs)))) ++ "]"
    show (AnyVM (Tuple (a, b))) = "{" ++ show (AnyVM a) ++ ", " ++ show (AnyVM b) ++ "}"
    show (AnyVM func@(UncalledFunction _ _)) = show (AnyAssembly func)
    show (AnyVM NULL) = "NULL"

instance Serializable Any where
    serialize :: Any -> Safe [Word8]
    serialize (Int n) = serialize n
    serialize (UInt n) = serialize n
    serialize (Char c) = serialize c
    serialize (Float f) = serialize f
    serialize (Bool b) = serialize b
    serialize (String str) = serialize str
    serialize EmptyArray = Value serializeTypeEmptyList
    serialize (Array xs) = serialize xs
    serialize (Tuple tuple) = serialize tuple
    serialize (UncalledFunction args returnType) = serialize (Func args returnType)
    serialize NULL = Value serializeTypeNull

class SafeOrd a where
    (~<=) :: a -> a -> Safe a
    (~<) :: a -> a -> Safe a
    (~>=) :: a -> a -> Safe a
    (~>) :: a -> a -> Safe a
    min' :: a -> a -> Safe a
    max' :: a -> a -> Safe a

toFloat :: Any -> Safe Float
toFloat (Int n) = Value (int2Float n)
toFloat (UInt n) = Value (int2Float (int n))
toFloat (Char c) = Value (int2Float $ fromIntegral $ c2w c)
toFloat (Float f) = Value f
toFloat (Bool b) = Value (fromBool b)
toFloat any' = Error (show any' ++ " isn't convertible to Float !")

commonType' :: Any -> Any -> (Float -> Any)
commonType' (Float _) _ = Float
commonType' _ (Float _) = Float
commonType' (UInt _) _ = UInt . word . float2Int
commonType' _ (UInt _) = UInt . word . float2Int
commonType' (Int _) _ = Int . int . i32 . float2Int
commonType' _ (Int _) = Int . int . i32 . float2Int
commonType' (Char _) _ = Char . w2c . (floorFloat :: Float -> Word8)
commonType' _ (Char _) = Char . w2c . (floorFloat :: Float -> Word8)
commonType' _ _ = Bool . (/= 0)

commonType :: Any -> Any -> Safe (Float -> Any)
commonType a b = toFloat a >> toFloat b >> Value (commonType' a b)

binaryBooleanOperator :: (Float -> Float -> Bool) -> Any -> Any -> Safe Any
binaryBooleanOperator f a b = liftA2 f (toFloat a) (toFloat b) <&> Bool

instance SafeOrd Any where
    (~<=) :: Any -> Any -> Safe Any
    (~<=) = binaryBooleanOperator (<=)

    (~<) :: Any -> Any -> Safe Any
    (~<) = binaryBooleanOperator (<)

    (~>=) :: Any -> Any -> Safe Any
    (~>=) = binaryBooleanOperator (>=)

    (~>) :: Any -> Any -> Safe Any
    (~>) = binaryBooleanOperator (>)

    min' :: Any -> Any -> Safe Any
    min' = binaryArithmeticOperator min

    max' :: Any -> Any -> Safe Any
    max' = binaryArithmeticOperator max

class SafeNum a where
    (~+) :: a -> a -> Safe a
    (~-) :: a -> a -> Safe a
    (~*) :: a -> a -> Safe a
    (~/) :: a -> a -> Safe a
    negate' :: a -> Safe a
    abs' :: a -> Safe a

binaryArithmeticOperator :: (Float -> Float -> Float) -> Any -> Any -> Safe Any
binaryArithmeticOperator f a b = liftA3 (\commonType_ a' b' -> commonType_ (f a' b')) (commonType a b) (toFloat a) (toFloat b)

unaryArithmeticOperator :: (Float -> Float) -> Any -> Safe Any
unaryArithmeticOperator f a = liftA2 (\commonType_ a' -> commonType_ (f a')) (commonType a a) (toFloat a)

instance SafeNum Any where
    (~+) :: Any -> Any -> Safe Any
    (~+) = binaryArithmeticOperator (+)

    (~-) :: Any -> Any -> Safe Any
    (~-) = binaryArithmeticOperator (-)

    (~*) :: Any -> Any -> Safe Any
    (~*) = binaryArithmeticOperator (*)

    (~/) :: Any -> Any -> Safe Any
    (~/) = binaryArithmeticOperator (/)

    negate' :: Any -> Safe Any
    negate' = unaryArithmeticOperator negate

    abs' :: Any -> Safe Any
    abs' = unaryArithmeticOperator abs

class SafeEq a where
    (~==) :: a -> a -> Safe a
    (~/=) :: a -> a -> Safe a

instance SafeEq Any where
    (~==) :: Any -> Any -> Safe Any
    (~==) = binaryBooleanOperator (==)

    (~/=) :: Any -> Any -> Safe Any
    (~/=) = binaryBooleanOperator (/=)

data Converter where
    Converter :: forall (a :: Data.Kind.Type). (Show a, Typeable a) => Proxy a -> Converter

instance Show Converter where
    show :: Converter -> String
    show (Converter proxy) = show (typeOf proxy)

haskellType :: Type -> Converter
haskellType T_Int = Converter (Proxy :: Proxy Int)
haskellType T_UInt = Converter (Proxy :: Proxy Word)
haskellType T_Char = Converter (Proxy :: Proxy Char)
haskellType T_Float = Converter (Proxy :: Proxy Float)
haskellType T_Bool = Converter (Proxy :: Proxy Bool)
haskellType (T_Tuple (a, b)) = case (haskellType a, haskellType b) of
    (Converter (_ :: Proxy a'), Converter (_ :: Proxy b')) -> Converter (Proxy :: Proxy (a', b'))
haskellType (T_List a) = case haskellType a of
    (Converter (_ :: Proxy a')) -> Converter (Proxy :: Proxy [a'])
haskellType T_EmptyList = Converter (Proxy :: Proxy [Int])
haskellType T_String = haskellType (T_List T_Char)
haskellType other = error ("Cannot convert from type " ++ show other ++ " !")

reduceList :: Eq a => String -> String -> [a] -> Safe a
reduceList emptyListError _ [] = Error emptyListError
reduceList _ valuesNotEqualError list@(a : _) = if all (== a) list then Value a else Error valuesNotEqualError

anyType :: Any -> Safe Type
anyType (Int _) = Value T_Int
anyType (UInt _) = Value T_UInt
anyType (Char _) = Value T_Char
anyType (Float _) = Value T_Float
anyType (Bool _) = Value T_Bool
anyType (String _) = Value T_String
anyType EmptyArray = Value T_EmptyList
anyType (UncalledFunction params returnType) = Value (T_Function params returnType)
anyType (Array xs) = mapM anyType xs >>= reduceList "Array must have at least 1 value, use EmptyArray instead !" "Array are homogeneous !" <&> T_List
anyType (Tuple (a, b)) = liftA2 (\a' b' -> T_Tuple (a', b')) (anyType a) (anyType b)
anyType NULL = Value T_NULL

makeAny :: forall a. (Typeable a, Show a) => Type -> a -> Safe Any
makeAny T_Int a = Int <$> (safeCast a :: Safe Int)
-- Either Word or Int depending on how it was called, if a was extracted from an AST,
-- it will be an Int (ASTUint holds an Int), but it it's from another Any, then UInt holds a Word
makeAny T_UInt a = UInt <$> ((safeCast a :: Safe Word) <|> ((safeCast a :: Safe Int) <&> word))
makeAny T_Char a = Char <$> (safeCast a :: Safe Char)
makeAny T_Float a = Float <$> (safeCast a :: Safe Float)
makeAny T_Bool a = Bool <$> (safeCast a :: Safe Bool)
makeAny T_NULL _ = Value NULL
makeAny T_String str = String <$> (safeCast str :: Safe String)
makeAny T_EmptyList _ = Value EmptyArray
makeAny (T_Function params returnType) _ = Value (UncalledFunction params returnType)
makeAny _type@(T_List elemType) list =
    case haskellType elemType of
        (Converter (_ :: Proxy a')) ->
            case haskellType _type of
                converter@(Converter (_ :: Proxy t)) ->
                    case eqT @t @[a'] of
                        Just _ -> (safeCast list :: Safe [a']) >>= mapM (makeAny elemType) <&> Array
                        Nothing -> Error ("'" ++ show list ++ "' of type " ++ show converter ++ " isn't a list !")

makeAny _type@(T_Tuple (t1, t2)) tuple =
    case haskellType t1 of
        (Converter (_ :: Proxy a1)) ->
            case haskellType t2 of
                (Converter (_ :: Proxy a2)) ->
                    case haskellType _type of
                        converter@(Converter (_ :: Proxy t)) ->
                            case eqT @t @(a1, a2) of
                                Just _ -> (safeCast tuple :: Safe (a1, a2)) >>= (\(a', b') -> liftA2 (\a'' b'' -> Tuple (a'', b'')) (makeAny t1 a') (makeAny t2 b'))
                                Nothing -> Error ("'" ++ show tuple ++ "' of type " ++ show converter ++ " isn't a tuple !")

makeAny _type _ = Error ("Type " ++ show _type ++ " cannot be converted to Any value !")