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
-- {-# LANGUAGE IncoherentInstances #-}

data Fix a = Fixed a | Unfixed deriving (Show, Eq)

isFixed (Fixed a) = True
isFixed _ = False

fromFixed (Fixed x) = x
fromFixed _ = error "fromFixed (Unfixed)"

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



class Fixable a where
  fixed :: a -> Bool
  unFix :: (a -> b) -> b

instance Fixable (Fix a) where
  fixed Unfixed = False
  fixed _       = True
  unFix f = f Unfixed

instance Fixable a where
  fixed = const True
  unFix f = f undefined


data FWrapper a b = FWrapper a b

class Resolution a
-- instance Resolution b => Resolution (FWrapper a b)
instance Resolution (FWrapper a b)


class Constant a
instance Eq a => Constant a
isConstant :: Constant a => a -> a
isConstant = id


class Resolvable a where
  resolve :: a -> b

class Resolution b => RFix a b where
  rfix :: a -> b

instance (Constant a, Resolution b) => RFix (Fix a -> b) b where
  rfix = unFix

instance (Constant a, RFix b c) => RFix (Fix a -> b) c where
  rfix = rfix . unFix






ff :: Show a => Fix a -> Fix a -> Fix a -> (b -> c -> d -> Bool, String)
ff x y z = (\x y z -> True, unwords $ map (++"|") $ map f [x,y,z])

gg :: (Show t, Fixable t) => t -> t -> t -> (t -> t -> t -> Bool, String)
gg x y z = (\a b c -> fixed a && fixed b && fixed c, unwords $ map f [x,y,z])

f :: (Show a, Fixable a) => a -> String
f x = if fixed x then show x else "nada"
-- f x | fixed x = show $ x
--     | True    = "nada"

f' :: (Show a, Fixable a) => a -> a -> String
f' x y = show $ map f [x,y]

g :: (Show a, Show a1, Show a2, Fixable a, Fixable a1, Fixable a2) => a -> a1 -> a2 -> String
g x y z = unwords [f x, f y, f z]

h :: (Num a, Fixable a) => a -> a
h x | fixed x = x + 1
    | True    = 1000


res1 :: (                    Fix a -> t) -> t
res2 :: (          Fix a1 -> Fix a -> t) -> t
res3 :: (Fix a2 -> Fix a1 -> Fix a -> t) -> t

res1 f =      (f Unfixed)
res2 f = res1 (f Unfixed)
res3 f = res2 (f Unfixed)




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


----------------------------------------------------------------------------------------------------
-- I dream of extentions...

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
----------------------------------------------------------------------------------------------------


