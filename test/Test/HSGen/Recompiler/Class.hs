module Test.HSGen.Recompiler.Class where

import Control.Spoon.Prim (throws)
import Data.Wrapped
import HSGen.Recompiler.Class

-- | These are for testing `resolve`
undefTo0 :: Num a => a -> a
undefTo0 x = if throws x
                then 0
                else x

undefTo1 :: Num a1 => a -> a1
undefTo1 x  | throws x = 1
            | otherwise     = 0

t1 :: Int -> Wrapped Int Int
t1 x = defWrap $ undefTo1 x

t2 :: Int -> Int -> Wrapped Int Int
t2 x y = defWrap $ undefTo1 x + undefTo1 y

t3 x y z = defWrap $ undefTo1 x + undefTo1 y + undefTo1 z

testResolve = undefined
testFixResolve = undefined

-- Test function:
-- example: plus $$ undefined $$ 3 == Wrap 3 3
plus :: Int -> Int -> Wrapped (Int -> Int -> Int) Int
plus x y = Wrap plus' (plus' x y)

plus' x y = undefTo0 x + undefTo0 y

toUndefined :: Bool -> a -> a
toUndefined xUndefined x  | xUndefined = undefined
                          | otherwise  = x

toZero :: Num a => Bool -> a -> a
toZero True _ = 0
toZero _    x = x

testPlus :: Bool -> Bool -> Int -> Int -> Bool
testPlus xUndefined yUndefined x y = it == whatItShouldBe
  where
    x' = toUndefined xUndefined x
    y' = toUndefined yUndefined y
    it = plus $$ x' $$ y'
    whatItShouldBe = let val = toZero xUndefined x + toZero yUndefined y in Wrap val val

-- | Not yet implemented tests:
-- undefTo1 x = if isUndefined x
--                 then 1
--                 else 0

-- c3undef :: Int -> Int -> Int -> Int
-- c3undef x y z = undefTo1 x + undefTo1 y + undefTo1 z

-- cund x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 = undefTo1 x0 + undefTo1 x1 + undefTo1 x2 + undefTo1 x3 + undefTo1 x4 + undefTo1 x5 + undefTo1 x6 + undefTo1 x7 + undefTo1 x8 + undefTo1 x9

-- wrs n = concat $ map ((" + undefTo1 x" ++) . show) [0..n]
-- vrs n = concat $ map ((" x" ++) . show) [0..n]
-- q n = "udf" ++ vrs n ++ " =" ++ (drop 2 $ wrs n)

