module SymbolTable (writeSymbolTable, readSymbolTable, symbolTableEnd, SymbolTable, noSymbolTableErrorMessage) where

import Data.ByteString.Internal (w2c)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing, fromJust)
import Data.Word (Word8)

import Bits (splitWord32, combineWord32)
import Serialize (serializeChar)
import Utils (Safe(..))
import VMData (Address)

type Symbol = (String, Address)
type SymbolTable = [Symbol]

symbolTableEnd :: [Word8]
symbolTableEnd = [0x11, 0x12, 0x13, 0x00]

encodeString :: String -> [Word8]
encodeString = (++ [0x00]) . concatMap serializeChar

writeSymbolTable :: SymbolTable -> [Word8]
writeSymbolTable = (++ symbolTableEnd) . concatMap writeSymbol
    where writeSymbol (sym, addr) = encodeString sym ++ splitWord32 addr

splitAtPattern :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitAtPattern [] list = Just (list, [])
splitAtPattern _ [] = Nothing
splitAtPattern pattern list@(x : xs)
    | isPrefixOf pattern list = Just ([], drop (length pattern) list)
    | isNothing splitBytes = Nothing
    | otherwise = Just (x : before, after)
    where splitBytes = splitAtPattern pattern xs
          (before, after) = fromJust splitBytes

splitAtElem :: Eq a => a -> [a] -> ([a], [a])
splitAtElem _ [] = ([], [])
splitAtElem a (x : xs)
    | a == x = ([], xs)
    | otherwise = (x : beforeA, afterA)
    where (beforeA, afterA) = splitAtElem a xs

readSymbol :: [Word8] -> (Symbol, [Word8])
readSymbol bytes = ((map w2c name, combineWord32 addr), rest')
    where (name, rest) = splitAtElem 0x00 bytes
          (addr, rest') = splitAt 4 rest

readSymbolTable' :: [Word8] -> SymbolTable
readSymbolTable' bytes
    | null rest = [symbol]
    | otherwise = symbol : readSymbolTable' rest
    where (symbol, rest) = readSymbol bytes

noSymbolTableErrorMessage :: String
noSymbolTableErrorMessage = "Symbol table not found !"

readSymbolTable :: [Word8] -> Safe (SymbolTable, [Word8])
readSymbolTable bytes
    | isNothing splitBytes = Error noSymbolTableErrorMessage
    | null table = Value ([], rest)
    | otherwise = Value (readSymbolTable' table, rest)
    where splitBytes = splitAtPattern symbolTableEnd bytes
          (table, rest) = fromJust splitBytes
