{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}



module Scratch where

import Data.Fixable
import Unsafe.Coerce
import Data.Default
import Data.Wrapped
import Data.Undefined

class Resolvable a where
  type Resolution a
  resolve :: a -> Resolution a

instance Resolvable (Wrapped a b) where
  type Resolution (Wrapped a b) = Wrapped a b
  resolve w = w

instance Resolvable a => Resolvable (t -> a) where
  type Resolution (t -> a) = Resolution a
  resolve w = resolve $ w undefined


class FixResolvable a where
  type FixResolution a
  fixResolve :: a -> FixResolution a

instance FixResolvable (Wrapped a b) where
  type FixResolution (Wrapped a b) = Wrapped a b
  fixResolve w = w

instance FixResolvable a => FixResolvable (Fix t -> a) where
  type FixResolution (Fix t -> a) = FixResolution a
  fixResolve w = fixResolve $ w Unfixed


u x = if isUndefined x then 1 else 0

t1 :: Int -> Wrapped (Int -> Int) Int
t1 x = defWrap $ u x

t2 :: Int -> Int -> Wrapped (Int -> Int -> Int) Int
t2 x y = defWrap $ u x + u y

t3 :: Int -> Int -> Int -> Wrapped (Int -> Int -> Int -> Int) Int
t3 x y z = defWrap $ u x + u y + u z

-- This is essentially scratch for a function to convert from infixr to infixl
--
-- E :: a -> a
-- \f g x -> f g x
-- id

-- E :: (a -> a1 -> a) -> a -> a1 -> a1 -> a
-- \f g x y -> f (f g x) y
-- (.) =<< (.)

-- E :: (b -> a -> b) -> b -> a -> a -> a -> b
-- \f g x y z -> f (f (f g x) y) z
-- ap ((.) . (.) . (.)) ((.) =<< (.))

-- E :: (b -> a -> b) -> b -> a -> a -> a -> a -> b
-- \f g x y z w -> f (f (f (f g x) y) z) w
-- ap ((.) . (.) . (.) . (.)) (ap ((.) . (.) . (.)) ((.) =<< (.)))

-- E :: (b -> a -> b) -> b -> a -> a -> a -> a -> a -> b
-- \f g x y z w q -> f (f (f (f (f g x) y) z) w) q
-- ap ((.) . (.) . (.) . (.) . (.)) (ap ((.) . (.) . (.) . (.)) (ap ((.) . (.) . (.)) ((.) =<< (.))))

-- g `f` x
-- (g `f` x) `f` y
-- ((g `f` x) `f` y) `f` z


-- (((g `f` x) `f` y) `f` z)

--         `f`
--         / \
--       `f`  z
--       / \
--     `f`   y
--     / \
--    g   x


