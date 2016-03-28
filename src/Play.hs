{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}CCCCCCCCCCCC

import Data.Proxy

-- Fixable CInt -> Fixable CInt -> Fixable CInt -> Either (Fixable CInt, Fixable CInt, Fixable CInt) CInt

--                 Fixable CInt -> Fixable CInt -> Either (              Fixable CInt, Fixable CInt) CInt

data F a = U deriving (Show, Eq)

class Fixable a where
  fixed :: a -> Bool
  unFix :: (a -> b) -> b

instance Fixable (Fix a) where
  fixed Unfixed = False
  fixed _       = True
  unFix f = f Unfixed

instance Fixable (F a) where
  fixed = const False
  unFix f = f undefined

instance Fixable a where
  fixed = const True
  unFix f = f undefined


data FWrapper a b = FWrapper a b

instance Eq a => Constant a
-- instance Show a => Constant a

instance Constant b => Constant (FWrapper a b)

isConstant :: Constant a => a -> a
isConstant = id

class Resolvable a where
  resolve :: a -> b

class RFix a b where
  rfix :: a -> b

instance (Constant a, Constant b) => RFix (Fix a -> b) b where
  rfix = unFix

instance (Constant a, RFix b c) => RFix (Fix a -> b) c where
  rfix = rfix . unFix

ff :: Num t => Fix t -> Fix t -> Fix t -> (t1 -> t2 -> t3 -> Bool, t)
ff x y z = (\x y z -> True, fromFixed x + fromFixed y + fromFixed z)

gg :: (Show t, Fixable t) => t -> t -> t -> (t -> t -> t -> Bool, String)
gg x y z = (\a b c -> fixed a && fixed b && fixed c, unwords $ map f [x,y,z])

f x = if fixed x then show x else "nada"

-- f x | fixed x = show $ x
--     | True    = "nada"

-- f' :: (Show a, Show b) => Fix a -> Fix b -> String
f' x y = show $ map f [x,y]


g x y z = unwords [f x, f y, f z]

h x | fixed x = x + 1
    | True    = 1000

instance Num a => Num (Fix a) where
  (+) Unfixed x = x
  (+) x Unfixed = x
  (+) (Fixed x) (Fixed y) = Fixed (x+y)

  (*) Unfixed x = x
  (*) x Unfixed = x
  (*) (Fixed x) (Fixed y) = Fixed (x*y)

  abs (Fixed x) = Fixed (abs x)
  abs _ = Unfixed

  signum (Fixed x) = Fixed (signum x)
  signum _ = Unfixed

  fromInteger x = Fixed (fromInteger x)

  negate (Fixed x) = Fixed (negate x)
  negate _ = Unfixed

-- class Fx a where
--   toFix :: a -> Fix a

-- instance Fx (Fix a) where
--   toFix (Fixed x) = Fixed x
--   toFix Unfixed   = Unfixed

-- instance Fx a where
--   toFix x = Fixed x

-- hi undefined = 0
-- hi x  = x

data Fix a = Fixed a | Unfixed deriving (Show, Eq)

isFixed (Fixed a) = True
isFixed _ = False

fromFixed (Fixed x) = x
fromFixed _ = error "fromFixed (Unfixed)"

class Constant a

-- <EvanR> type F a = a -> Int

res1 :: (Fix a -> t) -> t
res2 :: (Fix a1 -> Fix a -> t) -> t
res3 :: (Fix a2 -> Fix a1 -> Fix a -> t) -> t

res1 f = f Unfixed
res2 f = res1 (f Unfixed)
res3 f = res2 (f Unfixed)


-- f :: Fix a -> Fix b ->     c |>
--          a ->     b ->     c
-- f' x y = f (Fixed x) (Fixed y)

-- f ::  an0 -> an1 -> .. -> anm |>
--        .      .           .
--        .      .           .
--        ^      ^           |
--        |      |           v
--       a10 -> a11 -> .. -> a1m |>
--        ^      ^           |
--        |      |           v
--       a00 -> a01 -> .. -> a0m

-- fn = demote n $ f(n-1) (promote x0) (promote x1) .. (promote xm)
--  .          .    .               .            .              .
--  .          .    .               .            .              .
-- f2 = demote 2 $ f3     (promote x0) (promote x1) .. (promote xm)
-- f1 = demote 1 $ f2     (promote x0) (promote x1) .. (promote xm)
-- f0 =            f1     (promote x0) (promote x1) .. (promote xm)

-- f ::  .   ->   .        .
--       .   ->   .        .
--       ^   ->   ^        |
--       |        |        v
--     [[a]] -> [[b]] -> [[c]] |>
--       ^        ^        |
--       |        |        v
--      [a] ->   [b] ->   [c]

-- fn = join^n $ f{n-1} [x0] [x1]
--  .
--  .
-- f1 = join^2 $ f2     [x0] [x1]
-- f0 = join  $ f3     [x0] [x1]

-- f ::  .   ->     .          .
--       .   ->     .          .
--       ^   ->     ^          |
--       |          |          v
--   m (m a) -> m (m b) -> m (m c) |>
--       ^          ^          |
--       |          |          v
--      m a  ->    m b  ->    m c

-- fn = join^n $ f{n-1} (return x0) (return x1)
--  .
--  .
-- f1 = join^2 $ f2     (return x0) (return x1)
-- f0 = join  $ f3     (return x0) (return x1)

-- So we have:
-- - a rectangle of types, related by promotion/demotion
-- - a promoter
-- - a demoter

-- - argument xi is any from [a0i, a1i, ..]
-- - all arguments are stored
-- - upon return, all arguments are promoted to highest level of any argument
-- - return value is demoted
-- - actual function may either have the type of the highest level or an arbitrary level
-- - lower levels than defined are automatically generated
-- - promoter/demoter must have a simple inductive type

-- - There may be a promoter for *each* input type
-- - The promoter may be either default or specified


-- type FTo a b = a -> b

-- class R a where
--   res :: ((b -> c) ~ a) => a -> c

-- instance (Constant a, Constant b) => R (Fix a -> a -> b) where
--   res :: (Fix a -> a -> b) -> (a -> b)
--   res f = f Unfixed

-- instance (Constant a, Constant b, Constant c) => R (Fix a -> Fix b -> a -> b -> c) where
--   res f = undefined
--   res :: (Fix a -> Fix b -> a -> b -> c) -> (a -> b)
-- --   res :: (Fix a -> Fix b -> a -> b -> c) -> (

-- instance (Constant d, R (a -> b -> c)) => R (Fix d -> a -> d -> b -> c) where
--   res :: (Fix d -> a -> d -> b -> c) -> (d -> b -> c)
--   res f = res (f Unfixed)

-- class Uf a

-- instance Uf Int
-- instance Uf (Int -> Int)
-- instance Uf (Int -> Int -> Int)

-- class (Uf r) => S a r | a -> r where
--   ss :: (Uf b, a ~ (Fix b -> c)) => a -> r

-- instance S (Fix Int -> Int -> Int) (Int -> Int) where
--   ss f = f Unfixed

-- instance S (Fix Int -> Fix Int -> Int -> Int -> Int) (Int -> Int -> Int) where
--   ss f = f Unfixed Unfixed

-- instance (Uf a, Uf b) => S (Fix a -> b) b where
--   ss f = f Unfixed

-- instance (Uf a, Uf b, Uf c) => S (Fix a -> Fix b -> c) c  where
--   ss f = ss (f Unfixed)

t1 :: Fix Int -> Int -> Int
t1 (Fixed x) y = x
t1   _       y = (-1)

t2 :: Fix Int -> Fix Int -> Int -> Int -> Int
t2 (Fixed x) (Fixed y) z _ = x + y
t2 (Fixed x)  _        z _ = (-1) * x
t2    _      (Fixed y) z _ = (-10) * y
t2    _       _        _ _ = (-1000)

tt :: (Eq a, Eq b) => Fix a -> Fix b -> (Fix a, Fix b)
tt x y = (x,y)

-- import Prelude hiding ((.), ($))

-- class Functional f

-- instance Num a => Functional (a -> a)

-- instance Num a => Functional (a -> a -> a)

-- ($) :: Functional (a -> b) => (a -> b) -> a -> b
-- f $ x = f x

-- (.) :: (Functional (a -> b), Functional (b -> c)) => (b -> c) -> (a -> b) -> a -> c
-- (.) f g = \x -> f (g x)

-- -- (.) :: (b -> c) -> (a -> b) -> a -> c
-- -- ($) :: (a -> b) -> a -> b
