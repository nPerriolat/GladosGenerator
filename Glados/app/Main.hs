module Main (main) where

import Data.Maybe (isNothing, isJust, fromMaybe, fromJust)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)

import BinaryIO (writeBinary)
import Comment (deleteComment)
import Compile (compileAST)
import Parser (parse)
import ASTVerification (verifAST)
import Import (parseImport)
import Scanner
import Utils
import VM (mainVM)

data Args = Args {
    run :: Maybe Bool,
    compile :: Maybe Bool,
    file :: Maybe String,
    output :: Maybe String,
    help :: Bool
} deriving Show

defaultArgs :: Args
defaultArgs = Args {
    run = Nothing,
    compile = Nothing,
    file = Nothing,
    output = Nothing,
    help = False
}

mustShowHelp :: Args -> Bool
mustShowHelp args
    | help args = True
    | isNothing (run args) && isNothing (compile args) && isNothing (file args) = True
    | otherwise = False

processArg :: String -> Args -> Safe Args
processArg arg args
    | arg == "-r" || arg == "--run" = if isNothing (run args) then Value args { run = Just True } else Error "Run mode already specified !"
    | arg == "-c" || arg == "--compile" = if isNothing (compile args) then Value args { compile = Just True } else Error "Compile mode already specified !"
    | arg == "-h" || arg == "--help" = Value args { help = True }
    | isNothing (file args) = Value args { file = Just arg }
    | isNothing (output args) = Value args { output = Just arg }
    | otherwise = Error ("Both input and output file paths are already specified, got unexpected argument '" ++ arg ++ "' !")

processArgs :: [String] -> Safe Args
processArgs args = foldl (\args' arg -> args' >>= processArg arg) (Value defaultArgs) args

checkArgs :: Args -> Safe Args
checkArgs args
    | [run args, compile args] == [Just True, Just True] = Error "Both compile and run mode are specified !"
    | [run args, compile args] == [Nothing, Nothing] = Error "Neither compile mode nor run mode is specified !"
    | compile args == Just True && isNothing (file args) = Error "No file specified !"
    | isJust (run args) && isJust (output args) = Error "Output file is only relevant in compile mode !"
    | otherwise = Value args

safeToIO :: Safe a -> IO a
safeToIO (Error err) = die err
safeToIO (Value a) = pure a

helpMessage :: String
helpMessage =   "USAGE: ./glados [-c/--compile] | [-r/--run [-h/--help] <filename> [<output_filename>]\n\
                \\t-c/--compile : Compilation mode, turns a .pdp file into a binary file\n\
                \\t-r/--run : Run mode, the VM reads the compiled binary file and executes it\n\
                \\t-h/--help : Shows this screen\n\
                \\t<filename> : Input file path for the compiler or the VM (default if output.bin for the VM, no default for the compiler)\n\
                \\t<output_filename> : Output file path, optional and only valid in compile mode (default is output.bin)"

mainCompiler :: String -> String -> IO ()
mainCompiler filename outputFilename = do
    fileContent <- readFile filename
    fileImport <- parseImport (deleteComment fileContent)
    case fileImport of
        Error err -> die err
        Value content -> safeToIO ((verifAST $ parse $ scan (deleteComment content)) >>= compileAST) >>= (\(instructions, bytes) -> mapM print instructions >> pure bytes) >>= writeBinary outputFilename

main :: IO ()
main = do
    rawArgs <- getArgs
    let args = processArgs rawArgs
    case args of
        Error err -> die err
        Value args' -> case mustShowHelp args' of
            True -> putStrLn helpMessage >> exitSuccess
            False -> case checkArgs args' of
                Error err -> die err
                Value args'' -> do
                    let filename = file args''
                    if isJust (run args'') then mainVM (fromMaybe "output.bin" filename) else mainCompiler (fromJust filename) (fromMaybe "output.bin" (output args''))
