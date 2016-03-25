{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Prelude hiding ((.), ($))

class Functional f where
  getArg :: f -> a -> b

instance Num a => Functional a where
  getArg x a = []

instance Num a => Functional (a -> a) where
  -- getArg :: (a -> a) -> b -> c
  getArg f x = (x :) . getArg (f x)

($) :: Functional (a -> b) => (a -> b) -> a -> b
f $ x = f x

(.) :: (Functional (a -> b), Functional (b -> c)) => (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b
