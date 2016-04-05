{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module HSGen.Recomplier.Class (Compilable, ($$), ($$$), compile, flipCompilable,
                                resolve) where

import Data.Fix (Fix)
import Data.Wrapped (Wrapped)

-- | The `Compilable` class allows you to wrap a function in a
--  compiler/modifier. How? Suppose you have a function
--    > sum3 = \x y z -> x + y + z :: Num a => a -> a -> a -> a
--  and a method which simplifies `\x y z -> x + y + z` into
--  `\x y -> (x + y +)`, where `x + y` is evaluated exactly once instead of
--  every time the function is called. `Compilable` allows you to combine
--  these into a function:
--    > f' :: Num a -> Fix a -> Fix a -> Fix a -> Wrapped (a -> a -> a -> a) a
--  Where regular "fixed" arguments, such as `0`, are passed as `Fix 0` and "no
--  argument" is passed as `Unfixed`. When `f'` is evaluated, it returns a
--  `Wrapper` containing the function compiled, but with blank (`\ _ ->`)
--  arguments where `Fixed` were passed, and the result of evaluating the
--  function. This means that you can 1) use the function normally, but with
--  `Fixed` added to the arguments, 2) return a result of the return type
--  without applying all of the arguments, 3) utilize laziness to only compile
--  when the function from the `Wrapper` is called. Here's an example:
--  @
--  sum3 :: Fix Int -> Fix Int -> Fix Int -> Wrapper (Int -> Int -> Int -> Int) Int
--  sum3 x y z | isFixed x && isFixed y = Wrap (\__c -> (fromFixed x + fromFixed y) + c) undefined
--             | otherwise              = Wrap (\a b c -> a + b + c) (sum . map fromFixed $ [x,y,z])
--  @
--  Although not all cases are covered, and supposing that the
--  `(fromFixed x + fromFixed y)` is evaluated only once, `sum3 1 2` should be
--  equivalent to `(+ 3)`, as far as evaluation. (Although the poper order of
--  operations can be hard to ensure, the intended use case includes `IO` so this
--  is less of a concern).
--
class Compilable a r b s | a -> r, b ->s, a s -> b, r b -> a where
  -- | `resolve` has the effective type of
  --  `(Fix a0 -> .. -> Fix ai -> r) -> r`, which means it basically applies
  -- its input to `Unfixed` until it has type `Wrapped _ _`.
  resolve :: a -> r

  -- | `($$$) f wrappedFunction` essentially applies `f` directly to the
  --  `Wrapped` part of `wrappedFunction`.
  ($$$) :: (r -> s) -> a -> b

-- | `($$)` is analogous to `($)`, except that it takes a `Compilable`,
--  wrapped function.
($$) :: Compilable a (a1 -> s) (Fix a1 -> t) s => a -> a1 -> t
($$) f x = (($x) $$$ f) (Fixed x)

-- | Apply `flip` to both the wrapped (innter) and outer levels
flipCompilable = flip . (flip $$$)

-- | `compile` takes a `Compilable` and returns it compiled
--  (the wrapper and compiler are discarded)
compile :: _
compile = unwrapF . resolve

-- | `genInstance 0` returns the text for this instance
instance Compilable (Wrapped f r) (Wrapped f r) (Wrapped g s) (Wrapped g s) where
  resolve :: Wrapped f r -> Wrapped f r
  resolve w = w
  ($$$) :: (Wrapped f r -> Wrapped g s) -> Wrapped f r -> Wrapped g s
  ($$$) h w = h (w )

-- | `genInstance 1` returns the text for this instance
instance Compilable (Fix a1 -> Wrapped f r) (Wrapped f r) (Fix a1 -> Wrapped g s) (Wrapped g s) where
  resolve :: (Fix a1 -> Wrapped f r) -> Wrapped f r
  resolve w = w Unfixed
  ($$$) :: (Wrapped f r -> Wrapped g s) -> (Fix a1 -> Wrapped f r) -> (Fix a1 -> Wrapped g s)
  ($$$) h w = \x1 -> h (w x1)

-- | `genInstance 2` returns the text for this instance
instance Compilable (Fix a2 -> Fix a1 -> Wrapped f r) (Wrapped f r) (Fix a2 -> Fix a1 -> Wrapped g s) (Wrapped g s) where
  resolve :: (Fix a2 -> Fix a1 -> Wrapped f r) -> Wrapped f r
  resolve w = w Unfixed Unfixed
  ($$$) :: (Wrapped f r -> Wrapped g s) -> (Fix a2 -> Fix a1 -> Wrapped f r) -> (Fix a2 -> Fix a1 -> Wrapped g s)
  ($$$) h w = \x1 x2 -> h (w x1 x2)
