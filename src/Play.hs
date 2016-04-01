{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}

module Play where

import FixData

data FWrapper f c = FWrapper f c
instance Show b => Show (FWrapper a b) where
  showsPrec n (FWrapper x y) = showsPrec n y
  show        (FWrapper x y) = show        y

fWrap :: (a, b) -> FWrapper a b
fWrap (x,y) = FWrapper x y
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

 rbounce :: (b -> d) -> a -> c

rbounce' f b x = let y = rbounce f b in y $! x

class (Resolution b, Resolution d) => RBounce a b c d | a -> b, c -> d, a d -> c, b c -> a where
  rbounce :: (b -> d) -> a -> c

instance (Resolution b, Resolution d, b ~ FWrapper x y, d ~ FWrapper z w, a ~ b, c ~ d) => RBounce a b c d where
  rbounce :: (b -> d) -> a -> c
  rbounce = ($)

instance (Constant a, RBounce b c d e) => RBounce (Fix a -> b) c (Fix a -> d) e where
  rbounce f b = \x -> rbounce f (b x)
dum x = fWrap (const (isFixed x), isFixed x)
