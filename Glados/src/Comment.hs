module Comment (deleteComment) where

import Data.List (isPrefixOf)

removeTillNewLineOrComment :: String -> String
removeTillNewLineOrComment [] = []
removeTillNewLineOrComment [_] = []
removeTillNewLineOrComment str@(a:b:c)
        | isPrefixOf "\n" str = a : deleteComment (b:c)
        | isPrefixOf "COMMENT_DELIMITER" str = deleteComment c
        | otherwise = removeTillNewLineOrComment (b:c)

deleteComment :: String -> String
deleteComment [] = []
deleteComment str@(a:b:c)
        | isPrefixOf "COMMENT_DELIMITER" str = removeTillNewLineOrComment c
        | otherwise = a : deleteComment (b:c)
deleteComment (x:xs) = x : deleteComment xs
