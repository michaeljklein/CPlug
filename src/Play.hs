{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}

import FixData

data FWrapper f c = FWrapper f c
instance Show b => Show (FWrapper a b) where
  showsPrec n (FWrapper x y) = showsPrec n y
  show        (FWrapper x y) = show        y

fWrap :: (a, b) -> FWrapper a b
fWrap (x,y) = FWrapper x y

unwrapF :: FWrapper t t1 -> t
unwrapF (FWrapper f _) = f

unwrapC :: FWrapper t t1 -> t1
unwrapC (FWrapper _ c) = c

class Resolution a
instance Resolution (FWrapper a b)

isRes :: Resolution a => a -> Bool
isRes = const True

class Eq a => Constant a
instance Eq a => Constant a

class Resolution b => RFix a b | a -> b where
  rfix :: a -> b

instance (Resolution x, Resolution y, x ~ FWrapper a b, y ~ FWrapper c d, x ~ y) => RFix x y where
  rfix = id

instance (Constant a, Resolution b) => RFix (Fix a -> b) b where
  rfix :: Constant a => (Fix a -> b) -> b
  rfix =        ($ Unfixed)

instance (Constant a, RFix b c) => RFix (Fix a -> b) c where
  rfix :: Constant a => (Fix a -> b) -> c
  rfix = rfix . ($ Unfixed)

tt :: Fix Int -> FWrapper (Int -> String) String
tt x = fWrap (show, show x)

ttt :: Fix Int -> Fix Int -> FWrapper (Int -> Int -> String) String
ttt x y = FWrapper (\z w -> show (z, w)) (show (x, y))

sum3 :: Fix Int -> Fix Int -> Fix Int -> FWrapper (Int -> Int -> Int -> Int) Int
sum3 x y z = fWrap (\x2 y2 z2 -> fromF x x2 + fromF y y2 + fromF z z2, fromFixed x + fromFixed y + fromFixed z)
  where
    fromF Unfixed x = x
    fromF _       _ = 0


--  the depencency a b -> c means exactly that if we have instance1 a b c1 and instance2 a b c2 then c1 ~ c2.
--  For RBounce, we obviously have a -> b and c -> d, as b and d are particular resolutions, overdetermined by a and b.
--  What other dependencies do we have? Well, remember that we have
--    a ~ (Fix x0 -> Fix x1 -> .. -> b)
--    c ~ (Fix x0 -> Fix x1 -> .. -> d)
--  So a d -> c, c b -> a
--  To check that we have everything, lets write out all the possibilities:
--    a -> b, a -/> c, a -/> d, b -/> a, b -/> c, b -/> d, c -/> a, c -/> b, c -> d, d -/> a, d -/> b, d -/> c
--    a b -/> c, a b -/> d, a c -> b, a c -> d, a d -> b, a d -> c
--    b c -> a, b c -> d, b d -/> a, b d -/> c, c d -/> a, c d -/> b
--    a b c -> d, a b d -> c, a c b -> d, a c d -> b, b c d -> a
--  Now we cross out all the non-dependencies:
--    a -> b, c -> d
--    a c -> b, a c -> d, a d -> b, a d -> c
--    b c -> a, b c -> d
--    a b c -> d, a b d -> c, a c b -> d, a c d -> b, b c d -> a
--  Now we cross out all dependencies which are a weaker form of another (for example, cross out `a b c -> d` because we have the much stronger `c -> d`):
--    a -> b, c -> d
--    a d -> c
--    b c -> a
--  Much better. Now we can consolidate to the following class declaration:

-- class (Resolution b, Resolution d) => RBounce a b c d | a -> b, c -> d, a d -> c, b c -> a where
--   rbounce :: (b -> d) -> a -> c

class (Resolution b, Resolution d) => RBounce a b c d | a -> b, c -> d, a d -> c, b c -> a where
  rbounce :: (b -> d) -> a -> c


-- instance (Resolution x, Resolution y, Resolution z, Resolution w, x ~ FWrapper a b, y ~ FWrapper c d, z ~ FWrapper e f, w ~ FWrapper g h, x ~ y, z ~ w) => RBounce x y z w where
--   rbounce = ($)

-- instance (Resolution x, Resolution y, x ~ FWrapper a b, y ~ FWrapper c d, x ~ y, Resolution z, Resolution w, z ~ FWrapper e f, w ~ FWrapper g h, z ~ w) => RBounce x y z w where
--   rbounce = undefined

-- instance (RFix x y, RFix z w, x ~ y, y ~ z, z ~ w) => RBounce x y z w where
--   rbounce = undefined

-- instance (Resolution b, b ~ FWrapper x y, a ~ b, b ~ c, c ~ d) => RBounce a b c d where
--   rbounce f b = f b

instance (Resolution b, b ~ FWrapper x y, a ~ b) => RBounce a b c d where
  rbounce = ($)

instance (Constant a, RBounce b c d e) => RBounce (Fix a -> b) c (Fix a -> d) e where
  rbounce f b = \x -> rbounce f (b x)



-- Note that with this class declaration and instance, the following type is inferred by ghci:
-- rbounce id fWrap :: RBounce ((a, b) -> FWrapper a b) d c d => c
-- Note that although there is no instance for ((a -> b) -> FWrapper a b), the fact that any instance sayisfying
--  these constraints must have the same FWrapper type (because if id :: a -> b then a ~ b).


-- instance (Constant a, RBounce x y z w) => RBounce (Fix a -> x) y (Fix a -> z) w where
--   rbounce f b = \x -> rbounce f (b x)

dum x = fWrap (const (isFixed x), isFixed x)


-- I leave the old declaration below, because I might wonder how different it is after finding frustrations with the new one..
-- class (Resolution b, Resolution d) => RBounce a b c d | a -> b, c -> d, a c -> d where
--   rbounce :: (b -> d) -> a -> c







-- instance (Resolution x, Resolution y, x ~ FWrapper a b, y ~ FWrapper c d, x ~ y) => RBounce x y where
--   rbounce :: (Resolution w, w ~ FWrapper e f) => (y -> z) -> x -> w
--   rbounce f = f . rfix

-- sum3 U U U = (\x y z -> x + y + z, undefined)
-- sum3 1 U U = (\x y z -> 1 + y + z, 1        )
-- sum3 1 2 U = (\x y z -> 1 + 2 + z, 3        )
-- sum3 1 2 3 = (\x y z -> 1 + 2 + 3, 6        )

{-
rbounce ($1) sum3 = \x -> \y -> \z -> (($1) (\x2 y2 z2 -> x2 + y2 + z2), x + y + z)
rbounce flip sum3 = \x -> \y -> \z -> (flip (\x2 y2 z2 -> x2 + y2 + z2), x + y + z)
                  = \x ->             rbounce flip (sum3 x)
                  = \x -> \y ->       rbounce flip ((sum3 x) y)
                  = \x -> \y -> \z -> rbounce flip (((sum3 x) y) z)
                  = \x -> \y -> \z -> rbounce flip FWrapper (     sum3U) (x+y+z)
                  = \x -> \y -> \z ->              FWrapper (flip sum3U) (x+y+z)

rbounce flip sum3 = \x -> rbounce flip (sum3 x)
rbounce f b = \x -> rbounce f (b x)

\f b ->               (\x -> f   (b x)     )    :: (r1 -> r) -> (r2 -> r1            )             -> r2 -> r
\f b ->        (\y -> (\x -> f  ((b x) y)  ))   :: (r1 -> r) -> (r2 -> r3 -> r1      )       -> r3 -> r2 -> r
\f b -> (\z -> (\y -> (\x -> f (((b x) y) z)))) :: (r1 -> r) -> (r2 -> r3 -> r4 -> r1) -> r4 -> r3 -> r2 -> r

-}

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


-- This is a possible extention of Fix as a type class. What's really needed is a polytyped undefinedEq that can be used for pattern matching and otherwise returns an error.
-- class Fixable a where
--   fixed :: a -> Bool
--   unFix :: (a -> b) -> b

-- instance Fixable (Fix a) where
--   fixed Unfixed = False
--   fixed _       = True
--   unFix f = f Unfixed

-- instance Fixable a where
--   fixed = const True
--   unFix f = f undefined


-- I know it's not kosher to leave previous versions lying around in comments, but making the below work took a long time and I'm not willing to let it vanish into the commit history just yet.

-- class Resolution b => RFix a b | a -> b where
--   rfix :: a -> b

-- instance (Resolution x, Resolution y, x ~ FWrapper a b, y ~ FWrapper c d, x ~ y) => RFix x y where
--   rfix = id

-- instance Resolution b => RFix (Fix Int -> b) b where
--   rfix :: (Fix Int -> b) -> b
--   rfix =        ($ Unfixed)

-- instance RFix a b => RFix (Fix Int -> a) b where
--   rfix :: (Fix Int -> a) -> b
--   rfix = rfix . ($ Unfixed)
