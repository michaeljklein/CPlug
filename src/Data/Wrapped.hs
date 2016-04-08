module Data.Wrapped (Wrapped, unwrapF, unwrapR) where

-- | This is a wrapper type made specifically for `Compilable` instances.
-- 'a' should be a functional type and 'b' the \"last\" return type of that
-- function.
data Wrapped a b = Wrap a b deriving (Eq, Show, Ord)

-- | Unwrap a function
unwrapF :: Wrapped a b -> a
unwrapF (Wrap x _) = x

-- | Unwrap a return value
unwrapR :: Wrapped a b -> b
unwrapR (Wrap _ y) = y


