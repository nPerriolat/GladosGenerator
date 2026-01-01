{-# LANGUAGE NumericUnderscores #-}
module Bits (
    combineWord32,
    splitWord32,
    u32,
    i32,
    int,
    word,
    nthBit,
    setBit
) where

import Data.Bits ((.<<.), (.>>.), (.&.), (.|.))
import Data.Int (Int32)
import Data.Word (Word32, Word8)

splitWord32 :: Word32 -> [Word8]
splitWord32 w32 =   map fromIntegral [w32 .>>. 24
                    , (w32 .&. 0xFF_00_00) .>>. 16
                    , (w32 .&. 0xFF_00) .>>. 8
                    , w32 .&. 0xFF]

word :: Integral a => a -> Word
word = fromIntegral

u32 :: Integral a => a -> Word32
u32 = fromIntegral

i32 :: Integral a => a -> Int32
i32 = fromIntegral

int :: Integral a => a -> Int
int = fromIntegral

combineWord32 :: [Word8] -> Word32
combineWord32 [a, b, c, d] = (u32 a .<<. 24) .|. (u32 b .<<. 16) .|. (u32 c .<<. 8) .|. u32 d
combineWord32 xs = error $ "Expected 4 bytes but got " ++ show (length xs) ++ " (val: " ++ show xs ++ ") instead !"

nthBit :: Bool -> Int -> Word8
nthBit bool = (((if bool then 0x01 else 0x00) :: Word8) .<<.)

setBit :: Int -> Bool -> [Word8] -> [Word8]
setBit _ _ [] = []
setBit nth bit (x : xs)
    | nth < 0 = error $ "Bit index must be positive, but got " ++ show nth ++ " !"
    | nth < 8 = (x .&. (nthBit bit nth)) : xs
    | otherwise = x : setBit (nth - 8) bit xs