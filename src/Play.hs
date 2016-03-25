{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Prelude hiding ((.), ($))

class Functional f

instance Num a => Functional (a -> a)

instance Num a => Functional (a -> a -> a)

($) :: Functional (a -> b) => (a -> b) -> a -> b
f $ x = f x

(.) :: (Functional (a -> b), Functional (b -> c)) => (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b
