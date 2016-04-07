module Aux where

import Control.Monad (liftM2)

tupToList :: (a, a) -> [a]
tupToList (a,b) = [a,b]

-- | Illustrative example: `lastTup [1,2,3,4] == ([1,2,3], 4).
--  Used to parse input vars, ex: `lastTup ["unsigned", "int", "x"] == Just (["unsigned", "int"], "x")`.
lastTup :: [a] -> Maybe ([a], a)
lastTup [] = Nothing
lastTup x  = Just . liftM2 (,) init last $ x

apFst :: (a1 -> a2) -> (a1, a) -> (a2, a)
apFst f (a,b) = (f a, b)
