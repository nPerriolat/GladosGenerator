{-# LANGUAGE NumericUnderscores #-}
module Limits (
    checkInt,
    checkUInt,
    checkFloat
) where

uintMax :: Word
uintMax = 4_294_967_295

intMin :: Int
intMin = -2_147_483_648

intMax :: Int
intMax = 2_147_483_647

isInRange :: Ord a => a -> a -> a -> Bool
isInRange low high val
    | val < low = False
    | val > high = False
    | otherwise = True

checkInt :: Int -> Bool
checkInt int = isInRange intMin intMax int

checkUInt :: Word -> Bool
checkUInt uint = isInRange 0 uintMax uint

floatMin :: Float
floatMin = -3.4028237e38

floatMax :: Float
floatMax = 3.4028237e38

checkFloat :: Float -> Bool
checkFloat float = isInRange floatMin floatMax float
