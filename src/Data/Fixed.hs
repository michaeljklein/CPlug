module Data.Fixed where

-- | This is a analogue to `Maybe`, but without all of the predefined
-- instances, to prevent errors.
data Fix a = Unfixed | Fixed a deriving (Eq, Show, Ord)

-- | Illustrative examples:
--
-- >>> isFixed (Fixed "something")
-- True
--
-- >>> isFixed Unfixed
-- False
--
isFixed :: Fix a -> Bool
isFixed (Fixed _) = True
isFixed        _  = False

-- | `fromFixed` throws an error on `Unfixed`
fromFixed :: Fix a -> a
fromFixed (Fixed x) = x
