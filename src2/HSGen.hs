module HSGen where

import qualified Data.Text as T

--  HSGen
--

--  HSGen.FFI
--    raw function
--    piping caller function
--    export from module
--    {-# LANGUAGE ForeignFunctionInterface #-}
--    foreign import ccall unsafe "test.h cadd" c_add :: CInt -> CInt -> CInt


--  HSGen.Recompiler.Class
--    resolve
--    ($$$)

resolve :: a -> b
-- f $$$ c = apply f to the Wrapped return type of c
($$$) :: (b -> d) -> a -> c

-- c $$ x = apply c to (Fix x) and it's wrapped function to x
($$) c x = (($x) $$$ c) (Fix x)

-- apply flip to both the upper and lower levels
flipC = flip . (flip $$$)

class Compilable a where
  type Res r
  type Appd f
  type AppdRes fr
  resolve :: a -> Res r
  ($$$) :: (Res r -> AppdRes fr) -> a -> Appd f

instance
  type Res r = a
  type Appd f = b
  type AppdRes fr = b
  resolve :: a -> a
  resolve f = f
  ($$$)  :: (a -> b) -> a -> b
  ($$$) d f = d f

instance
  type Res r = b
  type Appd f = c
  type AppdRes fr = Fix a -> c
  resolve :: (Fix a -> b) -> b
  resolve f = f Unfixed
  ($$$)  :: (b -> c) -> (Fix a -> b) -> (Fix a -> c)
  ($$$) d f = \x -> d (f x)

instance
  resolve :: (Fix a -> Fix b -> c) -> c
  resolve f = f Unfixed Unfixed
  ($$$)  :: (c -> d) -> (Fix a -> Fix b -> c) -> (Fix a -> Fix b -> d)
  ($$$) d f = \x y -> d (f x y)

instance
  resolve :: (Fix a -> Fix b -> Fix c -> d) -> d
  resolve f = f Unfixed Unfixed Unfixed
  ($$$)  :: (d -> e) -> (Fix a -> Fix b -> Fix c -> d) -> (Fix a -> Fix b -> Fix c -> e)
  ($$$) d f = \x y z -> d (f x y z)

--  HSGen.Recompiler.Instances
--    generate instance code for HSGen.Recompiler.Class



