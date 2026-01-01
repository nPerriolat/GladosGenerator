module Hex (showHex32, showHex8) where

import Bits (u32)
import Data.Word (Word32, Word8)

showHex' :: Int -> Word32 -> String
showHex' nBytes n
    | nBytes == 0 = ""
    | n < 0x10 = ["0123456789ABCDEF" !! fromIntegral n]
    | otherwise = showHex' 1 (rem n 0x10) ++ showHex' (nBytes - 1) (div n 0x10)

showHex32' :: Word32 -> String
showHex32' = showHex' 4

showHex32 :: Word32 -> String
showHex32 = ("0x" ++) . reverse . showHex32'

showHex8 :: Word8 -> String
showHex8 = ("0x" ++) . reverse . (showHex' 1) . u32