module Data.Wrapped (Wrapped, unwrapC, unwrapF) where

data Wrapped a b = Wrap a b deriving (Eq, Show, Ord)

unwrapF :: Wrapped a b -> a
unwrapF (Wrap x _) = x

unwrapC :: Wrapped a b -> b
unwrapC (Wrap _ y) = y


