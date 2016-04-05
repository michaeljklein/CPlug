module Data.Fix (Fix (..), fromFixed, isFixed)

data Fix a = Unfixed | Fixed a deriving (Eq, Show, Ord)

isFixed :: Fix a -> Bool
isFixed (Fixed _) = True
isFixed        _  = False

fromFixed :: Fix a -> a
fromFixed (Fixed x) = x
