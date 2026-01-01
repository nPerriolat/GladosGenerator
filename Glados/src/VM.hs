{-# LANGUAGE TupleSections #-}

module VM (mainVM) where

import Data.Functor ((<&>))
import Data.List (singleton, genericSplitAt)
import Data.Word (Word8)
import System.Exit (die, exitWith, ExitCode (ExitSuccess))
import Unsafe.Coerce (unsafeCoerce)

import Any (Any(..), AnyVM(..), anyType)
import AssemblyInstructions (AssemblyInstruction(..), RegisterID)
import BinaryIO (readBinary)
import Bits (combineWord32, u32, int)
import Builtins (builtins)
import Data.ByteString.Internal (w2c)
import DataBuiltins (Symbols, BuiltinsSymbol(..))
import Deserialize (deserializeTypeAndValue, deserializeType, deserializeUInt, addBytesLen)
import SymbolTable (readSymbolTable, SymbolTable)
import Type (Type(..))
import Utils (Safe(..), boolToSafe, errorIf, bind2)
import VMData (Address, Vm(..), defaultVM)

pushRegister :: Maybe Any -> [Any] -> Safe [Any]
pushRegister (Just a) stack = Value $ a:stack
pushRegister Nothing _ = Error "Register empty"

endOfFile :: String
endOfFile = "Nothing left to do, closing VM."

pushValue :: Any -> [Any] -> Safe [Any]
pushValue a stack = Value $ a:stack

popStack :: RegisterID -> [Any] -> [Maybe Any] -> Safe ([Maybe Any], [Any])
popStack 0 (a : as) (_ : rs) = Value  (Just a : rs, as)
popStack x stack (r : rs) = popStack (x - 1) stack rs >>=(\(_reg, _stack) -> Value (r:_reg, _stack))
popStack _ [] _ = Error "Stack empty"
popStack _ _ [] = Error "Register out of bounds"

constructList :: Int -> Type -> [Any] -> Safe [Any]
constructList _ _type [] = Error "Not enough value in stack to perform construct"
constructList n _type xs = Value ((Array elems) : rest)
    where (elems, rest) = genericSplitAt n xs

constructTuple :: [Any] -> Type -> Type -> Safe [Any]
constructTuple (a : b : as) typeA typeB = checkTypes >> (Value $ (Tuple (a, b)) : as)
    where realTypeA = anyType a
          realTypeB = anyType b
          checkTypes = bind2 (\realTypeA' realTypeB' ->
            errorIf (== (typeA, typeB)) (errorMsg realTypeA' realTypeB') (Value (realTypeA', realTypeB'))) realTypeA realTypeB
          errorMsg realTypeA' realTypeB' = "construct: Type mismatch between expected type { " ++ show typeA ++ ", " ++ show typeB ++ " }, got { " ++ show realTypeA' ++ ", " ++ show realTypeB' ++ " } instead !"
constructTuple [_] _ _ = Error "Not enough value in stack to perform construct"
constructTuple _ _ _ = Error "Not enough value in stack to perform construct"

construct :: Type -> Int -> [Any] -> Safe [Any]
construct _ 0 _ = Error "Can't construct from 0 values !"
construct (T_List _type) size stack = constructList size _type stack
construct (T_Tuple (typeA, typeB)) 2 stack = constructTuple stack typeA typeB
construct (T_Tuple (_, _)) size _ = Error $ "Tried to construct tuple of size" ++ show size ++ "but tuples can only be of size 2"
construct _ _ _ = Error "Tried to use construct with wrong type"

moveValue :: RegisterID -> [Maybe Any] -> Any -> Safe [Maybe Any]
moveValue 0 (_ : rs) a = Value (Just a : rs)
moveValue x (r:rs) a = moveValue (x - 1) rs a >>=(\_reg -> Value (r:_reg))
moveValue  _ _ _ = Error "moveValue error"

moveRegister :: RegisterID -> RegisterID -> [Maybe Any] -> [Maybe Any] -> Safe [Maybe Any]
moveRegister 0 0 (_ : rs) (r : _) = Value (r : rs)
moveRegister 0 0 _ (Nothing : _) = Error "Source register empty"
moveRegister 0 y reg1 (_: rs') = moveRegister 0 (y - 1) reg1 rs'
moveRegister x 0 (r : rs) reg2 = moveRegister (x - 1) 0 rs reg2 >>=(\_reg -> Value (r:_reg))
moveRegister x y (r : rs) (_: rs') = moveRegister (x - 1) (y - 1) rs rs' >>=(\_reg -> Value (r:_reg))
moveRegister _ _ _ _ = Error "moveRegister error"

testReg :: [Any] -> Safe Bool
testReg [] = Error "Can't test on empty stack"
testReg ((Bool val):_)
                    | unsafeCoerce val :: Bool = Value True
                    | otherwise = Value False
testReg _ = Error "Register doesn't contain a boolean value"

jumpTrue :: Address -> Maybe Bool -> Safe Address
jumpTrue _ Nothing = Error "BF not set, please use test before conditional jump"
jumpTrue _ (Just False) = Value 0
jumpTrue addr (Just True) = Value addr

jumpFalse :: Address -> Maybe Bool -> Safe Address
jumpFalse _ Nothing = Error "BF not set, please use test before conditional jump"
jumpFalse _ (Just True) = Value 0
jumpFalse addr (Just False) = Value addr

searchBuiltins :: Symbols -> String -> Safe (Int, [Any] -> Safe Any)
searchBuiltins [] name = Error $ "*** ERROR: variable " ++ name ++ " is not bound."
searchBuiltins ((BackendBuiltins (str, nArgs, f)) : xs) name
                                | str == name = Value (nArgs, f)
                                | otherwise = searchBuiltins xs name

callBuiltins :: String -> [Any] -> Safe [Any]
callBuiltins name stack = case searchBuiltins builtins name of
    Error err -> Error err
    Value (nArgs, f)
        | length stack < nArgs -> Error ("Builtin " ++ name ++ " expects " ++ show nArgs ++ " arguments, but stack only has " ++ show (length stack) ++ " !")
        | otherwise -> f args <&> ((++ rest) . singleton)
            where (args, rest) = genericSplitAt nArgs stack

call :: String -> SymbolTable -> Safe Address
call str ((str', address):ts)
                | str == str' = Value address
                | otherwise = call str ts
call str [] = Error $ "*** ERROR: variable " ++ str ++ " is not bound."

returnRegister :: [Address] -> Maybe Any -> [Any] -> Safe ([Address], [Any], Address)
returnRegister (a:as) (Just val) stack = Value (as, val : stack, a)
returnRegister _ Nothing _ = Error "Empty register in return call"
returnRegister [] _ _ = Error "Not in a function cannot return"

returnValue :: [Address] -> Any -> [Any] -> Safe ([Address], [Any], Address)
returnValue (a:as) val stack = Value (as, val : stack, a)
returnValue [] _ _ = Error "Not in a function cannot return"

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

executeInstruction' :: AssemblyInstruction -> SymbolTable -> Vm -> Safe (Vm, Maybe Any)
executeInstruction' (PushRegister registerID) _ (Vm (reg:rs) cstack bf vstack pc) = pushRegister (reg !! fromIntegral registerID) vstack >>=(\_stack -> Value (Vm (reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (PushValue value) _ (Vm (reg:rs) cstack bf vstack pc) = pushValue value vstack >>=(\ _stack -> Value (Vm (reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (Pop registerID) _ (Vm (reg:rs) cstack bf vstack pc) = popStack registerID vstack reg >>=(\(_reg, _stack) -> Value (Vm (_reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (Construct _type _size) _ (Vm (reg:rs) cstack bf vstack pc) = construct _type _size vstack >>=(\_stack -> Value (Vm (reg:rs) cstack bf _stack pc, Nothing))
executeInstruction' (Test) _ (Vm (reg:rs) cstack _ (tested:vstack) pc) = testReg (tested:vstack) >>=(\_bf -> Value (Vm (reg:rs) cstack (Just _bf) vstack pc, Nothing))
executeInstruction' (Jump addr) _ (Vm reg cstack bf vstack pc) = Value (Vm reg cstack bf vstack (pc + addr), Nothing) 
executeInstruction' (JumpIfTrue addr) _ (Vm (reg:rs) cstack bf vstack pc) = jumpTrue addr bf >>=(\move -> Value (Vm (reg:rs) cstack bf vstack (pc + move), Nothing))
executeInstruction' (JumpIfFalse addr) _ (Vm (reg:rs) cstack bf vstack pc) = jumpFalse addr bf >>=(\move -> Value (Vm (reg:rs) cstack bf vstack (pc + move), Nothing))
executeInstruction' (Call name) table (Vm reg cstack bf vstack pc)= case call name table of  
                                                                    Value _address -> Value (Vm (replicate 16 Nothing : reg) (pc:cstack) bf vstack _address, Nothing)
                                                                    Error _ -> callBuiltins name vstack >>=(\_stack -> Value (Vm reg cstack bf _stack pc, Nothing))
executeInstruction' (RetRegister registerID) _ (Vm (reg:rs) cstack bf vstack _) = returnRegister cstack (reg !! fromIntegral registerID) vstack >>=(\(_cstack, _vstack, _address) -> Value (Vm rs _cstack bf _vstack _address, Nothing))
executeInstruction' (RetValue value) _ (Vm (_:rs) cstack bf vstack _)= returnValue cstack value vstack >>=(\(_cstack, _vstack, _address) -> Value (Vm rs _cstack bf _vstack _address, Nothing))
executeInstruction' Ret _ (Vm (_:rs) (_address:_cstack) bf vstack _) = Value (Vm rs _cstack bf vstack _address, Nothing)
executeInstruction' (MovRegister register1 register2) _ (Vm (reg:rs) cstack bf vstack pc) = moveRegister register1 register2 reg reg >>=(\_reg -> Value (Vm (_reg:rs) cstack bf vstack pc, Nothing))
executeInstruction' (MovValue registerID value) _ (Vm (reg:rs) cstack bf vstack pc) =  moveValue registerID reg value >>=(\_reg -> Value (Vm (_reg:rs) cstack bf vstack pc, Nothing))
executeInstruction' (OutRegister registerID) _ (Vm (reg:rs) cstack bf vstack pc) = Value (Vm (reg:rs) cstack bf vstack pc, reg !! fromIntegral registerID)
executeInstruction' (OutValue value) _ vm = Value (vm, Just value)
executeInstruction' _ _ vm = Error ("Instruction not recognized " ++ show vm)

executeInstruction :: AssemblyInstruction -> SymbolTable -> Vm -> Safe (Vm, Maybe Any)
executeInstruction instruction = executeInstruction' instruction

parseInstruction' :: [Word8] -> Safe (AssemblyInstruction, Int)
parseInstruction' (0x00 : xs) = mapFst PushValue <$> addBytesLen 1 <$> deserializeTypeAndValue xs
parseInstruction' (0x01 : reg : _) = Value (PushRegister reg, 2)
parseInstruction' (0x10 : reg : _) = Value (Pop reg, 2)
parseInstruction' (0x20 : xs) = deserializeType xs >>=(\(_type, len, rest) -> deserializeUInt rest >>= \(a, len', _) -> Value (Construct _type (int a), len + len' + 1))
parseInstruction' (0x30 : _) = Value (Test, 1)
parseInstruction' (0x40 : byte1 : byte2 : byte3 : byte4 : _) = Value (JumpIfTrue (combineWord32 [byte1, byte2, byte3, byte4]), 5)
parseInstruction' (0x50 : byte1 : byte2 : byte3 : byte4 : _) = Value (JumpIfFalse (combineWord32 [byte1, byte2, byte3, byte4]), 5)
parseInstruction' (0x60 : xs) = name' <&> ((, length name + 2) . Call) -- +2 for leading 0x60 and trailing 0x00
    where hasNullCharacter = errorIf (elem 0x00) "Unterminated call function name !" (Value xs)
          name = map w2c $ takeWhile (/= 0x00) xs
          name' = hasNullCharacter >>= boolToSafe "Empty function name in Call instruction !" (name /= []) >> Value name
parseInstruction' (0x70 : xs) = mapFst RetValue <$> addBytesLen 1 <$> deserializeTypeAndValue xs
parseInstruction' (0x71 : reg : _) = Value (RetRegister reg, 2)
parseInstruction' (0x72 : _) = Value (Ret, 1)
parseInstruction' (0x80 : reg : xs) = mapFst (MovValue reg) <$> addBytesLen 2 <$> deserializeTypeAndValue xs
parseInstruction' (0x81 : reg1 : reg2 : _) = Value (MovRegister reg1 reg2, 3)
parseInstruction' (0x90 : xs) = mapFst OutValue <$> addBytesLen 1 <$> deserializeTypeAndValue xs
parseInstruction' (0x91 : reg : _) = Value (OutRegister reg, 2)
parseInstruction' (0xA0 : byte1 : byte2 : byte3 : byte4 : _) = Value (Jump (combineWord32 [byte1, byte2, byte3, byte4]), 5)
parseInstruction' _ = Error endOfFile

movePc :: Int -> Vm -> Vm
movePc increment vm = vm {
    _pc = _pc vm + u32 increment
}

parseInstruction :: Vm -> [Word8] -> SymbolTable -> Safe (Vm, Maybe Any)
parseInstruction vm bytes table = parseInstruction' (drop (fromIntegral $ _pc vm) bytes) >>=(\(instruction, movement) -> executeInstruction instruction table (movePc movement vm))

parseFile :: Vm -> SymbolTable -> [Word8] -> IO ()
parseFile vm table bytes = case parseInstruction vm bytes table of
                        Error "Nothing left to do, closing VM." -> exitWith ExitSuccess
                        Error err -> die err
                        Value (_vm, Nothing) -> parseFile _vm table bytes
                        Value (_vm, Just a) -> print (AnyVM a) >> parseFile _vm table bytes

mainVM :: FilePath -> IO ()
mainVM path = do
    file <- readBinary path
    case readSymbolTable file of
        Error err -> die err
        Value (table, rest) -> case call "__main" table of 
                Value _address -> parseFile (defaultVM _address) table rest
                Error err -> die err
            -- writeBinary "output.txt"
