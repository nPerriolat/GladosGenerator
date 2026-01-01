{-# LANGUAGE TupleSections #-}

module Deserialize (deserialize, deserializeTypeAndValue, deserializeType, deserializeList, deserializeInt, addBytesLen, deserializeUInt) where

import Data.Bits (Bits, (.<<.), (.|.))
import Data.ByteString.Internal (w2c)
import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Word (Word8)
import GHC.Float (castWord32ToFloat)

import Any (Any(..))
import Bits (u32, i32, int, combineWord32)
import Hex (showHex8)
import Type (Type(..))
import Utils (Safe(..), errorIf, mapFst3)

toAnyAndBytes :: (a -> Any) -> (a, Int, [Word8]) -> (Any, Int, [Word8])
toAnyAndBytes anyfy (val, len, bytes) = (anyfy val, len, bytes)

deserialize :: Type -> [Word8] -> Safe (Any, Int, [Word8])
deserialize T_Bool bytes = deserializeBool bytes <&> toAnyAndBytes Bool
deserialize T_Char bytes = deserializeChar bytes <&> toAnyAndBytes Char
deserialize T_Int bytes = deserializeInt bytes <&> toAnyAndBytes Int
deserialize T_UInt bytes = deserializeUInt bytes <&> toAnyAndBytes UInt
deserialize T_NULL bytes = deserializeTypeNull bytes <&> toAnyAndBytes (const NULL)
deserialize T_Float bytes = deserializeFloat bytes <&> toAnyAndBytes Float
deserialize T_EmptyList bytes = deserializeTypeEmptyList bytes <&> toAnyAndBytes (const EmptyArray)
deserialize _type@(T_List _) bytes = deserializeList _type bytes
deserialize _type@(T_Tuple _) bytes = deserializeTuple _type bytes
deserialize (T_Function params ret) bytes = Value (UncalledFunction params ret, 0, bytes)
deserialize a _ = Error ("Deserializing " ++ show a ++ " isn't implemented for now !")

deserializeBool :: [Word8] -> Safe (Bool, Int, [Word8])
deserializeBool (0x00 : xs) = Value (False, 1, xs)
deserializeBool (0x01 : xs) = Value (True, 1, xs)
deserializeBool (a : _) = Error ("Got unexpected byte " ++ show a ++ " while trying to deserialize a boolean !")
deserializeBool [] = Error "Cannot deserialize a boolean, no byte to read !"

deserializeChar :: [Word8] -> Safe (Char, Int, [Word8])
deserializeChar (x : xs) = Value (w2c x, 1, xs)
deserializeChar [] = Error "Cannot deserialize a char, no byte to read !"

deserializeInt' :: Bits a => (Word8 -> a) -> [Word8] -> Safe (a, Int, [Word8])
deserializeInt' fromIntegral' (b1 : b2 : b3 : b4 : bytes) = Value (int', 4, bytes)
    where int' = ((fromIntegral' b1) .<<. 24) .|. ((fromIntegral' b2) .<<. 16) .|. ((fromIntegral' b3) .<<. 8)  .|. (fromIntegral' b4)
deserializeInt' _ bytes = error ("Cannot deserialize an int/uint, less than 4 bytes to read (got " ++ show (length bytes) ++ " bytes : " ++ show bytes ++ ") !")

deserializeInt :: [Word8] -> Safe (Int, Int, [Word8])
deserializeInt bytes = deserializeInt' i32 bytes <&> (\(a, b, c) -> (fromIntegral a, b, c))

deserializeUInt :: [Word8] -> Safe (Word, Int, [Word8])
deserializeUInt bytes = deserializeInt' u32 bytes <&> (\(a, b, c) -> (fromIntegral a, b, c))

deserializeTypeNull :: [Word8] -> Safe (Int, Int, [Word8])
deserializeTypeNull (0x09 : xs) = Value (0x00, 1, xs) -- 0x00 is a dummy value
deserializeTypeNull _ = error "Cannot deserialize NULL type, no byte to read !"

deserializeFloat :: [Word8] -> Safe (Float, Int, [Word8])
deserializeFloat (b1 : b2 : b3 : b4 : bytes) = Value (castWord32ToFloat float, 4, bytes)
    where float = (u32 b1 .<<. 24) .|. (u32 b2 .<<. 16) .|. (u32 b3 .<<. 8)  .|. u32 b4
deserializeFloat bytes = Error ("Cannot deserialize a float, less than 4 bytes to read (got " ++ show (length bytes) ++ " bytes) !")

deserializeTypeCombinationTypes :: Int -> [Word8] -> Safe ([Type], Int, [Word8])
deserializeTypeCombinationTypes 0 list = deserializeType list >>=(\(_type, size, rest) -> Value ([_type], size, rest))
deserializeTypeCombinationTypes _ [] = Error "Not enough element to deserialize combination type"
deserializeTypeCombinationTypes x list = deserializeType list >>=(\(_type, size, rest) -> deserializeTypeCombinationTypes (x - 1) rest >>=(\(_list, _size, _rest') -> Value (_type:_list, size + _size, _rest')))

deserializeType :: [Word8] -> Safe (Type, Int, [Word8])
deserializeType (0x01 : xs) = Value (T_Bool, 1, xs)
deserializeType (0x02 : xs) = Value (T_Int, 1, xs)
deserializeType (0x03 : xs) = Value (T_UInt, 1, xs)
deserializeType (0x04 : xs) = Value (T_Float, 1, xs)
deserializeType (0x05 : xs) = Value (T_Char, 1, xs)
deserializeType (0x06 : xs) = deserializeType xs >>=(\(type1, size1, rest) -> deserializeType rest >>=(\(type2, size2, rest') -> Value (T_Tuple (type1, type2), 1 + size1 + size2, rest')))
deserializeType (0x07 : xs) = deserializeType xs >>=(\(_type, _size, rest) -> Value (T_List _type, _size + 1, rest))
deserializeType (0x08 : xs) = deserializeInt xs >>=(\(_value, len, rest)-> deserializeTypeCombinationTypes _value rest >>= (\(_type, _size, _rest) -> Value (T_Combination _type, _size + 1 + len, _rest)))
deserializeType (0x0A : xs) = deserializeTypeEmptyList xs
deserializeType (0x0B : a : b : c : d : xs) = deserializeFunction (int $ combineWord32 [a, b, c, d]) xs
deserializeType bytes = Value (T_NULL, 1, bytes)

deserializeFunction :: Int -> [Word8] -> Safe (Type, Int, [Word8])
deserializeFunction n bytes = deserializeTypeCombinationTypes (n - 1) bytes >>= \(params, n', rest) -> deserializeType rest <&> (\(ret, n'', rest') -> (T_Function params ret, n' + n'' + 5, rest'))

deserializeTypeEmptyList :: [Word8] -> Safe (Type, Int, [Word8])
deserializeTypeEmptyList (0x0A : rest) = Value (T_EmptyList, 1, rest)
deserializeTypeEmptyList [] = Error "Cannot deserialize an empty list, no byte to read !"
deserializeTypeEmptyList (a : _) = Error ("Cannot deserialize an empty list, bad byte (got " ++ showHex8 a ++ "instead of 0x0A) !")

deserializeList' :: Type -> Int -> [Word8] -> Safe (Any, Int, [Word8])
deserializeList' _ 0 _ = Error "deserializeList' : no element to deserialize !"
deserializeList' _type n bytes = foldl (\previous _ -> previous >>= deserializeNext) firstElem [2..n]
    where firstElem = deserialize _type bytes <&> mapFst3 (Array . singleton)

          combine :: Int -> [Any] -> (Any, Int, [Word8]) -> (Any, Int, [Word8])
          combine len list (newElem, len', rest) = (Array (list ++ [newElem]), len + len', rest)

          deserializeNext :: (Any, Int, [Word8]) -> Safe (Any, Int, [Word8])
          deserializeNext ((Array list), len, rest) = deserialize _type rest <&> combine len list
          deserializeNext (any', _, _) = Error ("Unexpectedly deserialized a " ++ show any' ++ " instead of an array !")

deserializeList :: Type -> [Word8] -> Safe (Any, Int, [Word8])
deserializeList _ [] = Error "Cannot deserialize a list, no byte to read !"
deserializeList (T_List _type) bytes = listLen >>= (\(len, bytesLen, rest) -> deserializeList' _type (fromIntegral len) rest <&> (\(list', bytesLen', rest'') -> (list', bytesLen + bytesLen', rest'')))
    where listLen = errorIf (\(val, _, _) -> val /= 0) "Use deserializeEmptyList for empty lists !" (deserializeUInt bytes)
deserializeList _type _ = Error ("Cannot deserialize a list of type " ++ show _type ++ ", T_List ... was expected.")

deserializeTuple :: Type -> [Word8] -> Safe (Any, Int, [Word8])
deserializeTuple _ [] = Error "Cannot deserialize a tuple, no byte to read !"
deserializeTuple (T_Tuple (a, b)) bytes = deserialize a bytes >>= (\(a', len, bytes') -> deserialize b bytes' <&> \(b', len', bytes'') -> (Tuple (a', b'), len + len', bytes''))
deserializeTuple _type _ = Error ("Cannot deserialize a tuple of type " ++ show _type ++ ", T_Tuple (..., ...) was expected.")

deserializeTypeAndValue :: [Word8] -> Safe (Any, Int)
deserializeTypeAndValue bytes = deserializeType bytes >>= (\(_type, len, rest) -> deserialize _type rest <&> \(a, len', _) -> (a, len + len'))

addBytesLen :: Int -> (Any, Int) -> (Any, Int)
addBytesLen n (val, len) = (val, len + n)