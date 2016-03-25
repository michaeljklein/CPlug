{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding ((.), ($))

class Functional f where
  unwrap :: f -> a
  wrap   :: a -> f

($) :: Functional (a -> b) => (a -> b) -> a -> b
f $ x = f x

(.) :: (Functional (a -> b), Functional (b -> c)) => (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b
