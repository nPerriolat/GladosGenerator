{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}

module Serialize (
    Serializable,
    serialize,
    serializeInt,
    serializeUInt,
    serializeChar,
    serializeFloat,
    serializeBool,
    serializeTuple,
    serializeList,
    serializeType,
    serializeTypeInt,
    serializeTypeUInt,
    serializeTypeChar,
    serializeTypeFloat,
    serializeTypeBool,
    serializeTypeTuple,
    serializeTypeList,
    serializeTypeEmptyList,
    serializeTypeCombination,
    serializeTypeNull,
    Func(..)
) where


import Control.Applicative (liftA3)
import Data.Bits ((.>>.))
import Data.ByteString.Internal (c2w)
import Data.Functor ((<&>))
import Data.Word (Word8, Word32)
import GHC.Float (castFloatToWord32)

import Bits (splitWord32, word)
import Limits (checkInt, checkUInt, checkFloat)
import Type (Type(..))
import Utils (Safe(..), concatMapM)

data Func = Func [Type] Type

class Serializable a where
    serialize :: a -> Safe [Word8]

instance Serializable Bool where
    serialize = Value . serializeBool

instance Serializable Int where
    serialize = serializeInt

instance Serializable Word where
    serialize = serializeUInt

instance Serializable Float where
    serialize = serializeFloat

instance Serializable Char where
    serialize = Value . serializeChar

instance (Serializable a, Serializable b) => Serializable (a, b) where
    serialize = serializeTuple

instance Serializable a => Serializable [a] where
    serialize = serializeList

instance Serializable Func where
    serialize _ = Value serializeTypeNull

serializeInt' :: Word32 -> [Word8]
serializeInt' value =
    [ fromIntegral (value .>>. 24) :: Word8
    , fromIntegral (value .>>. 16) :: Word8
    , fromIntegral (value .>>. 8) :: Word8
    , fromIntegral value :: Word8 ]

serializeInt :: Int -> Safe [Word8]
serializeInt int
    | not (checkInt int) = Error "Out of range int !"
    | otherwise = Value (serializeInt' (fromIntegral $ int :: Word32))

serializeUInt :: Word -> Safe [Word8]
serializeUInt uint
    | not (checkUInt uint) = Error "Negative uint !"
    | otherwise = Value (serializeInt' (fromIntegral uint :: Word32))

serializeChar :: Char -> [Word8]
serializeChar char = [c2w char]

serializeFloat' :: Float -> [Word8]
serializeFloat' float = splitWord32 (castFloatToWord32 float)

serializeFloat :: Float -> Safe [Word8]
serializeFloat float
    | not (checkFloat float) = Error "Out of range float !"
    | otherwise = Value (serializeFloat' float)

serializeBool :: Bool -> [Word8]
serializeBool bool = [if bool then 0x01 else 0x00]

serializeTuple :: (Serializable a, Serializable b) => (a, b) -> Safe [Word8]
serializeTuple (x, y) = liftA2 (++) (serialize x) (serialize y)

serializeList' :: (Serializable a) => [a] -> Safe [Word8]
serializeList' (x:xs) = liftA2 (++) (serialize x) (serializeList' xs)
serializeList' [] = Value []

serializeList :: (Serializable a) => [a] -> Safe [Word8]
serializeList xs = liftA2 (++) (serializeUInt (word $ length xs)) (serializeList' xs)

-- serializeCombination :: [Type] -> [Word8]
-- serializeCombination list = serializeList list

serializeType :: Type -> Safe [Word8]
serializeType T_Int = Value serializeTypeInt
serializeType T_UInt = Value serializeTypeUInt
serializeType T_Float = Value serializeTypeFloat
serializeType T_Bool = Value serializeTypeBool
serializeType T_Char = Value serializeTypeChar
serializeType T_EmptyList = Value serializeTypeEmptyList
serializeType T_String = serializeTypeList T_Char
serializeType T_NULL = Value serializeTypeNull
serializeType (T_Tuple types) = serializeTypeTuple types
serializeType (T_List elemType) = serializeTypeList elemType
serializeType (T_Combination types) = serializeTypeCombination types
serializeType (T_Function params returnType) = serializeFunctionType params returnType
serializeType _type = Error ("Cannot serialize type " ++ show _type)

serializeTypeInt :: [Word8]
serializeTypeInt = [0x02]

serializeTypeUInt :: [Word8]
serializeTypeUInt = [0x03]

serializeTypeChar :: [Word8]
serializeTypeChar = [0x05]

serializeTypeFloat :: [Word8]
serializeTypeFloat = [0x04]

serializeTypeBool :: [Word8]
serializeTypeBool = [0x01]

serializeTypeTuple :: (Type, Type) -> Safe [Word8]
serializeTypeTuple (x, y) = liftA2 (++) (serializeType x) (serializeType y) <&> ([0x06] ++)

serializeTypeList :: Type -> Safe [Word8]
serializeTypeList t = serializeType t <&> ([0x07] ++)

serializeTypeEmptyList :: [Word8]
serializeTypeEmptyList = [0x0A]

serializeTypeCombination' :: [Type] -> Safe [Word8]
serializeTypeCombination' (x:xs) = liftA2 (++) (serializeType x) (serializeTypeCombination' xs)
serializeTypeCombination' [] = Value []

serializeTypeCombination :: [Type] -> Safe [Word8]
serializeTypeCombination [] = Error "Empty Combination !"
serializeTypeCombination list = liftA2 (++) (serializeInt (length list)) (serializeTypeCombination' list) <&> ([0x08] ++)

serializeTypeNull :: [Word8]
serializeTypeNull = [0x09]

serializeFunctionType :: [Type] -> Type -> Safe [Word8]
serializeFunctionType params returnType = liftA3 (\nParams' params'' returnType'' -> [0x0B] ++ nParams' ++ params'' ++ returnType'') nParams params' returnType'
    where nParams = serializeUInt (word $ length params)
          params' = concatMapM serializeType params
          returnType' = serializeType returnType