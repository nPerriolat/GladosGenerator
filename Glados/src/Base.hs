module Base (processToken) where

import Data.Char (ord)
import Data.List (isPrefixOf)
-- import Data.Maybe (fromMaybe)
import Utils


-- | Converts a number from a given base to decimal
toDecimal :: Int -> String -> Either String Int
toDecimal base numStr
    | base < 2 || base > 36 = Left "Base must be between 2 and 36."
    | not (all isValidDigit numStr) = Left $ "Invalid digit in input for base " ++ show base ++ ": " ++ numStr
    | otherwise = Right $ foldl (\acc x -> acc * base + charToValue x) 0 numStr
  where
    charToValue c
        | c >= '0' && c <= '9' = ord c - ord '0'
        | c >= 'a' && c <= 'z' = ord c - ord 'a' + 10
        | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 10
        | otherwise = -1 -- Should never happen if isValidDigit is correct

    isValidDigit c =
        let value = charToValue c
        in value >= 0 && value < base

-- | Processes a token, replacing base-prefixed numbers with their decimal equivalent
processToken :: String -> Safe String
processToken token
    | "Ob" `isPrefixOf` token = Value $ replaceWithDecimal 2 (drop 2 token)
    | "Oo" `isPrefixOf` token = Value $ replaceWithDecimal 8 (drop 2 token)
    | "Od" `isPrefixOf` token = Value $ replaceWithDecimal 10 (drop 2 token)
    | "Ox" `isPrefixOf` token = Value $ replaceWithDecimal 16 (drop 2 token)
    | take 2 token == "O{" = processCustomBase token
    | otherwise = Error "Basic String" -- si Erreur "Basic String" est retourné, c'est à dire que la string a rien a voir avec les bases
  where
    replaceWithDecimal base numStr = 
        case toDecimal base numStr of
            Right decimal -> show decimal
            Left _ -> token

    processCustomBase token' =
        case span (/= '}') (drop 2 token') of
            (baseStr, '}' : numStr) ->
                case reads baseStr of
                    [(base, "")] -> Value $ replaceWithDecimal base numStr
                    _ -> Error "Not a Base Number"
            _ -> Error "Not a Base Number"
