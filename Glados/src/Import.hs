module Import (parseImport) where

import Utils (Safe(..))
import Control.Exception (try, IOException)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)

-- Read and validate a file safely
processFile :: FilePath -> IO (Safe String)
processFile filepath = do
    exists <- doesFileExist filepath
    if not exists
        then return $ Error ("GLaDOS: ImportError: File \"" ++ filepath ++ "\" does not exist.")
        else do
            result <- try (readFile filepath) :: IO (Either IOException String)
            return $ case result of
                Left _      -> Error ("GLaDOS: ImportError: Error reading file \"" ++ filepath ++ "\".")
                Right content -> Value content

-- Handle import lines recursively
processLine :: [String] -> IO (Safe String)
processLine ["IMPORT", filepath] = processFile filepath
processLine _ = return $ Error "GLaDOS: ImportError: Invalid import line format. Expected 'import <filepath>'."

-- Process lines and replace imports
catchImport :: [String] -> IO (Safe String)
catchImport [] = return $ Value ""
catchImport (line:rest)
    | "IMPORT" `isPrefixOf` line =
        case words line of
            ["IMPORT", _] -> do
                importResult <- processLine (words line)
                case importResult of                                    -- handle import
                    Error err -> return $ Error err
                    Value importContent -> do
                        processedImport <- parseImport importContent
                        case processedImport of                         -- process import of import (import inside import)
                            Error err -> return $ Error err
                            Value parsedImport -> do
                                restResult <- catchImport rest
                                case restResult of
                                    Error err -> return $ Error err
                                    Value restContent -> return $ Value (parsedImport ++ "\n" ++ restContent)
            _ -> return $ Error "GLaDOS: ImportError: Import line must contain exactly one file path."
    | otherwise = do
        restResult <- catchImport rest
        case restResult of
            Error err -> return $ Error err
            Value restContent -> return $ Value (line ++ "\n" ++ restContent)

-- Parse the input string and handle imports
parseImport :: String -> IO (Safe String)
parseImport input = catchImport (lines input)
