module FixData where

data Fix a = Fixed a | Unfixed deriving (Show, Eq)

isFixed (Fixed a) = True
isFixed _ = False

fromFixed (Fixed x) = x
fromFixed _ = error "fromFixed (Unfixed)"

instance Ord a => Ord (Fix a) where
  compare (Unfixed) (Unfixed) = EQ
  compare (Unfixed) (Fixed y) = LT
  compare (Fixed x) (Unfixed) = GT
  compare (Fixed x) (Fixed y) = compare x y

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


