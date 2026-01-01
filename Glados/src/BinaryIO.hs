module BinaryIO (Word8, readBinary, writeBinary) where

import qualified Data.ByteString as Byte
import Data.Word (Word8)

readBinary :: FilePath -> IO [Word8]
readBinary filepath = fmap Byte.unpack (Byte.readFile filepath)

writeBinary :: FilePath -> [Word8] -> IO ()
writeBinary filepath content = Byte.writeFile filepath (Byte.pack content)