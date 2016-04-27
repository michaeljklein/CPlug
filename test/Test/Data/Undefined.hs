module Test.Data.Undefined where

import Data.Undefined
import Test.QuickCheck (quickCheck)

simpleCheck :: a -> IO ()
simpleCheck x = do
  if isUndefined x
  then
    do
      print "simpleCheck passes"
  else
    do
      print "simpleCheck fails"

generalTest :: [Integer] -> Bool
generalTest = not . isUndefined

sumTestListIsZero :: Int -> Bool
sumTestListIsZero n = if n >= 0
                         then 0 == sum [if isUndefined x then 0 else 0 | x <- [0..n]]
                         else True

testIn :: Int -> (Int, Int)
testIn y = if even y
              then (0, y)
              else (undefined, y)

testOut :: (Int, Int) -> Int
testOut (a,b) = if isUndefined a
                   then 0 - b - 1
                   else b

sumTestListIsZero2 :: Int -> Bool
sumTestListIsZero2 n = if n >= 0
                          then 0 == sum ([testOut . testIn $ y | y <- [1..n + mod n 2]])
                          else True

testDataUndefined = do
  simpleCheck undefined
  simpleCheck (undefined :: String)
  simpleCheck (undefined :: Bool)
  simpleCheck (undefined :: Integer)
  simpleCheck (undefined :: [Bool])
  simpleCheck (undefined :: IO Int)
  print "generalTest"
  quickCheck generalTest
  print "sumTestListIsZero"
  quickCheck sumTestListIsZero
  print "sumTestListIsZero2"
  quickCheck sumTestListIsZero2

