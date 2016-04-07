module Data.Either.Aux where

fromLeft  :: Either a b -> Maybe a
fromLeft  (Left  x) = Just x
fromLeft   _        = Nothing

fromRight :: Either a b -> Maybe b
fromRight (Right x) = Just x
fromRight  _        = Nothing

mapLeft :: (t -> a) -> [Either t b] -> [Either a b]
mapLeft _ [] = []
mapLeft f (x@(Left  y) : xs) = Left (f y) : mapLeft f xs
mapLeft f (x@(Right y) : xs) = Right   y  : mapLeft f xs

unsafeFromLeft :: Either a b -> a
unsafeFromLeft (Left x) = x
unsafeFromLeft _        = error "called unsafeFromLeft on (Right x)"


