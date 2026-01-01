{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Utils
    ( Safe(Value, Error),
    printSafe,
    printSafeList,
    concatSStrings,
    joinSStrings,
    isValue,
    mapFst,
    mapFst3,
    mapTuple,
    tupleConcat,
    alternativeMap,
    maybeToSafe,
    boolToSafe,
    safeCast,
    errorIf,
    bind2,
    bind3,
    concatMapM,
    fromSafe
    ) where

import Control.Applicative ((<|>), empty, Alternative, liftA3)
import Control.Monad (join)
import Data.Functor ((<&>))
import Data.Typeable (Typeable, cast, typeOf)

data Safe a = Value a | Error String deriving (Eq, Ord)

instance Functor Safe where
    fmap f (Value a) = Value (f a)
    fmap _ (Error err) = Error err

instance Applicative Safe where
    pure = Value
    (Value f) <*> (Value a) = Value (f a)
    (Error err) <*> _ = Error err
    _ <*> (Error err) = Error err
    (Value _) *> b = b
    (Error err) *> _ = Error err

instance Alternative Safe where
    (<|>) first (Error _) = first
    (<|>) (Error _) second = second
    (<|>) first@(Value _) (Value _) = first

    empty = Error ""

instance Monad Safe where
    (Value a) >>= f = f a
    (Error err) >>= _ = (Error err)
    (>>) = (*>)
    return = pure

instance Show a => Show (Safe a) where
    show (Error err) = show err
    show (Value a) = show a

printSafe :: Show a => Safe a -> IO ()
printSafe (Error err) = putStrLn err
printSafe (Value a) = print a

printSafeList :: Show a => Safe [a] -> IO ()
printSafeList (Error err) = putStrLn err
printSafeList (Value []) = putStrLn ""
printSafeList (Value [x]) = print x
printSafeList (Value (x:xs)) = print x >> printSafe (Value xs)

isValue :: Safe a -> Bool
isValue (Value _) = True
isValue _ = False

fromSafe :: Safe a -> a
fromSafe (Value a) = a
fromSafe (Error err) = error ("Attempted to unwrap a Safe but got error '" ++ err ++ "' instead !")

maybeToSafe :: String -> Maybe a -> Safe a
maybeToSafe err Nothing = Error err
maybeToSafe _ (Just a) = Value a

boolToSafe :: String -> Bool -> a -> Safe a
boolToSafe _ True a = Value a
boolToSafe err False _ = Error err

errorIf :: (a -> Bool) -> String -> Safe a -> Safe a
errorIf f err a = a >>= (\a' -> if f a' then Value a' else Error err)

safeCast :: forall a b. (Typeable a, Typeable b) => a -> Safe b -- forall allows undefined :: b, and ScopedVariables allows forall here
safeCast a = maybeToSafe errorMsg (cast a)
    where errorMsg = "Cannot cast value of type " ++ show (typeOf a) ++ " into type " ++ show (typeOf (undefined :: b)) ++ " !"

type SString = Safe String

concatSStrings :: SString -> SString -> SString
concatSStrings (Error err1) (Error err2) = Error ("2 Errors encountered at the same time: " ++ err1 ++ " ; " ++ err2)
concatSStrings (Error _) other = other
concatSStrings first (Error _) = first
concatSStrings (Value first) (Value other) = Value $ concat [first, other]

joinSStrings :: String -> [SString] -> SString
joinSStrings sep = foldr (\a b -> concatSStrings a (concatSStrings (b *> Value sep) b)) (Error "Cannot join those strings.")

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapFst3 :: (a -> d) -> (a, b, c) -> (d, b, c)
mapFst3 f (a, b, c) = (f a, b, c)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

tupleConcat :: ([a], [b], [c]) -> ([a], [b], [c]) -> ([a], [b], [c])
tupleConcat (a, b, c) (a', b', c') = (a ++ a', b ++ b', c ++ c')

alternativeMap :: (a -> b) -> b -> Safe a -> b
alternativeMap f _default a = fromSafe ((f <$> a) <|> (Value _default))

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = join (liftA2 f a b)

bind3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bind3 f a b c = join (liftA3 f a b c)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = mapM f xs <&> concat