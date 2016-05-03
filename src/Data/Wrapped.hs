{-# LANGUAGE InstanceSigs #-}

module Data.Wrapped where

import Control.Applicative (Applicative(..))
import Data.Default

-- | This is a wrapper type made specifically for `Compilable` instances.
-- 'a' should be a functional type and 'b' the \"last\" return type of that
-- function. In all instances, `a` is considered a kind of metadata (as it
-- should only be accessed by compiling functions).
data Wrapped a b = Wrap a b

defWrap :: Default a => b -> Wrapped a b
defWrap = Wrap def

-- | Unwrap the function
unwrapF :: Wrapped a b -> a
unwrapF (Wrap x _) = x

-- | Unwrap the return value
unwrapR :: Wrapped a b -> b
unwrapR (Wrap _ y) = y

-- | Synonym for `unwrapR`
unwrap :: Wrapped a b -> b
unwrap = unwrapR

-- | `wrappedAp` or `wrapped function apply`
wrappedAp :: Wrapped (t -> a) b -> t -> Wrapped a b
wrappedAp (Wrap f y) x = Wrap (f x) y

-- | This note applies to all the instances for `Wrapped a b`:
-- These instances ignore `a`, unless it's required for the type, for example
-- with `Bounded`, in which case `a = Data.Default.def`
instance Eq b => Eq (Wrapped a b) where
  (==) (Wrap x y) (Wrap z w) = y == w

instance Ord b => Ord (Wrapped a b) where
  compare (Wrap x y) (Wrap z w) = compare y w

instance (Default a, Enum b) => Enum (Wrapped a b) where
  toEnum n = defWrap (toEnum n)
  fromEnum (Wrap _ y) = fromEnum y

instance (Default a, Bounded b) => Bounded (Wrapped a b) where
  minBound = defWrap minBound
  maxBound = defWrap maxBound

instance Functor (Wrapped a) where
  fmap :: (a1 -> b) -> Wrapped a a1 -> Wrapped a b
  fmap f (Wrap t x) = Wrap t (f x)

instance Default a => Applicative (Wrapped a) where
  pure :: a1 -> Wrapped a a1
  pure = defWrap

  (<*>) :: Wrapped a (a1 -> b) -> Wrapped a a1 -> Wrapped a b
  (<*>) (Wrap _ f) (Wrap t x) = Wrap t (f x)

instance (Default a, Default b) => Default (Wrapped a b) where
  def = defWrap def

instance Show b => Show (Wrapped a b) where
  showsPrec n (Wrap _ y) = showsPrec n y
  show        (Wrap _ y) = show y
