module Data.Either.Aux where

-- | This provides safe unpacking of a `Left` object.
fromLeft  :: Either a b -> Maybe a
fromLeft  (Left  x) = Just x
fromLeft   _        = Nothing

-- | This provides safe unpacking of a `Right` object.
fromRight :: Either a b -> Maybe b
fromRight (Right x) = Just x
fromRight  _        = Nothing

-- | This maps a function over a list, only applying it to `Left` objects
mapLeft :: (t -> a) -> [Either t b] -> [Either a b]
mapLeft _ []               = []
mapLeft f ((Left  y) : xs) = Left (f y) : mapLeft f xs
mapLeft f ((Right y) : xs) = Right   y  : mapLeft f xs

-- | This maps a function over a list, only applying it to `Right` objects
mapRight :: (t -> b) -> [Either a t] -> [Either a b]
mapRight _ []               = []
mapRight f ((Right y) : xs) = Right (f y) : mapRight f xs
mapRight f ((Left  y) : xs) = Left     y  : mapRight f xs

-- | This will throw an error if its input is `Right`
unsafeFromLeft :: Either a b -> a
unsafeFromLeft (Left x) = x
unsafeFromLeft _        = error "called unsafeFromLeft on (Right x)"


