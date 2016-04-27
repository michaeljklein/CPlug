{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE RankNTypes #-}

module HSGen.Recompiler.Class where

import Control.Monad (ap)
import Data.Default.Aux
import Data.Fixable (Fix(..))
import Data.Undefined
import Data.Wrapped (Wrapped(..), unwrapF, wrappedAp, defWrap)

infixl 1 $$
infixr 2 $$$
infixr 2 ###


-- | This class is of functions with type `a` and final return type `r`.
-- Additionally, `r` should be of the form: `Wrapped` a b
class Resolvable a r | a -> r where
  -- | `resolve` resolves a "`Resolvable` a r" to `r`
  resolve :: a -> r

instance Resolvable (Wrapped a b) (Wrapped a b) where
  resolve :: Wrapped a b -> Wrapped a b
  resolve w = w

instance (Resolvable a r) => Resolvable (t -> a) r where
  resolve :: (t -> a) -> r
  resolve w = resolve $ w undefined


class Compilable a r b s | a -> r, b -> s, a s -> b, r b -> a where
  -- | `($$$) f wrappedFunction` essentially applies `f` directly to the
  --  `Wrapped` part of `wrappedFunction`.
  ($$$) :: (r -> s) -> a -> b

instance Compilable (Wrapped a b) (Wrapped a b) (Wrapped c d) (Wrapped c d) where
  ($$$) :: (Wrapped a b -> Wrapped c d) -> Wrapped a b -> Wrapped c d
  ($$$) f = f

instance (Compilable a r b s) => Compilable (t -> a) r (t -> b) s where
  ($$$) :: (r -> s) -> (t -> a) -> t -> b
  (f $$$ w) x = f $$$ w x

-- | Given a simple `Compilable` function:
--
-- @
--    plus :: Int -> Int -> Wrapped (Int -> Int -> Int) Int
-- @
--
-- You can use `($$)` to apply `plus` to both the outer (`Int` -> `Int` -> ..)
-- and innner (`Wrapped` (`Int` -> ..)) functions of `plus`. For example:
--
-- >>> plus $$ 1 $$ 2
-- Wrap 3 3
--
($$) :: Compilable a1 (Wrapped (t -> a) b) t1 (Wrapped a b) => (t -> a1) -> t -> t1
($$) w x = flip wrappedAp x $$$ w $ x


-- | See `Resolvable`
class FixResolvable a r | a -> r where
  fixResolve :: a -> r

instance FixResolvable (Wrapped a b) (Wrapped a b) where
  fixResolve :: Wrapped a b -> Wrapped a b
  fixResolve w = w

instance (FixResolvable a r) => FixResolvable (Fix t -> a) r where
  fixResolve :: (Fix t -> a) -> r
  fixResolve w = fixResolve $ w Unfixed

-- | See `Compilable`
class FixCompilable a r b s | a -> r, b -> s, a s -> b, r b -> a where
  (###) :: (r -> s) -> a -> b

instance FixCompilable (Wrapped a b) (Wrapped a b) (Wrapped c d) (Wrapped c d) where
  (###) :: (Wrapped a b -> Wrapped c d) -> Wrapped a b -> Wrapped c d
  (###) f = f

instance (FixCompilable a r b s) => FixCompilable (Fix t -> a) r (Fix t -> b) s where
  (###) :: (r -> s) -> (Fix t -> a) -> Fix t -> b
  (f ### w) x = f ### w x

-- | See `($$)`
(##) :: FixCompilable
       a (Wrapped (r -> a1) b) (r -> r1) (Wrapped a1 b) =>
     a -> r -> r1
(##) w x = flip wrappedAp x ### w $ x


-- | Apply a function to a `Compilable` from the outside
-- (just regular application) and also inside (`($$$)`)
apInOut :: Compilable a b b c => (b -> c) -> a -> c
apInOut = ap (.) ($$$)

-- | See `apInOut`
fixApInOut :: FixCompilable a b b c => (b -> c) -> a -> c
fixApInOut = ap (.) (###)


-- | Flip a compilable function
flipC :: Compilable a1 (a -> b -> c) c (b -> a -> c) =>
     (a -> b -> a1) -> b -> a -> c
flipC = apInOut flip

-- | See `flipC`
fixFlipC :: FixCompilable a (a1 -> b -> c) (a1 -> b -> c) (b -> a1 -> c) =>
     a -> b -> a1 -> c
fixFlipC = fixApInOut flip


-- | Compile a compilable function
compile :: Resolvable a (Wrapped f r) => a -> f
compile = unwrapF . resolve

-- | See `compile`
fixCompile :: FixResolvable a (Wrapped f r) => a -> f
fixCompile = unwrapF . fixResolve

