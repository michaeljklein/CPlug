{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}

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


data FWrapper a b = FWrapper a b
instance Show b => Show (FWrapper a b) where
  showsPrec n (FWrapper x y) = showsPrec n y
  show        (FWrapper x y) = show        y

fwrap (x,y) = FWrapper x y
unwrapF (FWrapper f _) = f
unwrapC (FWrapper _ c) = c

class Resolution a
-- instance Resolution b => Resolution (FWrapper a b)
instance Resolution (FWrapper a b)

isRes :: Resolution a => a -> Bool
isRes = const True

class Eq a => Constant a
instance Eq a => Constant a
isConstant :: Constant a => a -> a
isConstant = id





class Resolution b => RFix' a b | a -> b where
  rfix' :: a -> b

instance (Resolution x, Resolution y, x ~ FWrapper a b, y ~ FWrapper c d, x ~ y) => RFix' x y where
  rfix' = id

instance (Constant a, Resolution b) => RFix' (Fix a -> b) b where
  rfix' :: Constant a => (Fix a -> b) -> b
  rfix' = ($ Unfixed)

instance (Constant a, RFix' b c) => RFix' (Fix a -> b) c where
  rfix' :: Constant a => (Fix a -> b) -> c
  rfix' = rfix' . ($ Unfixed)

tt :: Fix Int -> FWrapper (Int -> String) String
tt x = fwrap (show, show x)

ttt :: Fix Int -> Fix Int -> FWrapper (Int -> Int -> String) String
ttt x y = FWrapper (\z w -> show (z, w)) (show (x, y))


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

-- tt :: (Eq a, Eq b) => Fix a -> Fix b -> (Fix a, Fix b)
-- tt x y = (x,y)

-- I know it's not kosher to leave previous versions lying around in comments, but making the below work took a long time and I'm not willing to let it vanish just yet.

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
