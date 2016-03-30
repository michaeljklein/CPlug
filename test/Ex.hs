

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Maybe (isJust)

data Wrapper a = Wrapper a deriving (Show)

class Resolution a
instance Resolution (Wrapper a)

class (Resolution b, Resolution d) => C a b c d | a -> b, c -> d, a d -> c, b c -> a where
  cfun :: (b -> d) -> a -> c

instance (Resolution b, Resolution d, a ~ b, c ~ d) => C a b c d where
  cfun = ($)

instance (Eq a, C b c d e) => C (Maybe a -> b) c (Maybe a -> d) e where
  cfun f b = \x -> cfun f (b x)

foo = Wrapper . isJust


-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE OverlappingInstances #-}

-- import Data.Maybe (isJust)

-- data Wrapper a = Wrapper a deriving (Show)

-- class Resolution a
-- instance Resolution (Wrapper a)

-- class (Resolution b, Resolution d) => C a b c d | a -> b, c -> d, a d -> c, b c -> a where
--   cfun :: (b -> d) -> a -> c

-- instance (Resolution b, Resolution d, a ~ b, c ~ d) => C a b c d where
--   cfun = ($)

-- instance (Eq a, C b c d e) => C (Maybe a -> b) c (Maybe a -> d) e where
--   cfun f b = \x -> cfun f (b x)

-- dum = Wrapper . isJust



-- import Data.Maybe (isJust)

-- data Wrapper f c = Wrapper f c deriving (Show)

-- class Resolution a
-- instance Resolution (Wrapper a b)

-- class (Resolution b, Resolution d) => C a b c d | a -> b, c -> d, a d -> c, b c -> a where
--   cfun :: (b -> d) -> a -> c

-- instance (Resolution b, Resolution d, a ~ b, c ~ d) => C a b c d where
--   cfun = ($)

-- instance (Eq a, C b c d e) => C (Maybe a -> b) c (Maybe a -> d) e where
--   cfun f b = \x -> cfun f (b x)

-- dum x = Wrapper (isJust x) (isJust x)


{-
data Fix a = Fixed a | Unfixed deriving (Show, Eq)

isFixed (Fixed a) = True
isFixed _ = False

isJust (Just _) = True
isJust _        = False

data Wrapper f c = Wrapper f c deriving (Show)

class Resolution a
instance Resolution (Wrapper a b)

class Eq a => Constant a
instance Eq a => Constant a

class (Resolution b, Resolution d) => RBounce a b c d | a -> b, c -> d, a d -> c, b c -> a where
  rbounce :: (b -> d) -> a -> c

instance (Resolution b, Resolution d, b ~ Wrapper x y, d ~ Wrapper z w, a ~ b, c ~ d) => RBounce a b c d where
  rbounce = ($)

instance (Constant a, RBounce b c d e) => RBounce (Fix a -> b) c (Fix a -> d) e where
  rbounce f b = \x -> rbounce f (b x)

dum x = Wrapper (isFixed x) (isFixed x)
-}

-- data Fix a = Fixed a | Unfixed deriving (Show, Eq)

-- isFixed (Fixed a) = True
-- isFixed _ = False

-- data FWrapper f c = FWrapper f c deriving (Show)

-- fWrap :: (a, b) -> FWrapper a b
-- fWrap (x,y) = FWrapper x y

-- unwrapC :: FWrapper t t1 -> t1
-- unwrapC (FWrapper _ c) = c

-- class Resolution a
-- instance Resolution (FWrapper a b)

-- class Eq a => Constant a
-- instance Eq a => Constant a

-- class (Resolution b, Resolution d) => RBounce a b c d | a -> b, c -> d, a d -> c, b c -> a where
--   rbounce :: (b -> d) -> a -> c

-- instance (Resolution b, Resolution d, b ~ FWrapper x y, d ~ FWrapper z w, a ~ b, c ~ d) => RBounce a b c d where
--   rbounce :: (b -> d) -> a -> c
--   rbounce = ($)

-- instance (Constant a, RBounce b c d e) => RBounce (Fix a -> b) c (Fix a -> d) e where
--   rbounce f b = \x -> rbounce f (b x)

-- dum x = fWrap ((isFixed x), isFixed x)
