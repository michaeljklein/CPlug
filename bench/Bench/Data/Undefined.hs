module Bench.Data.Undefined where

import Criterion.Main
import Data.Undefined (isUndefined)
import Data.Fixable (Fix(..), isFixed)

benchDataUndefined :: IO ()
benchDataUndefined = defaultMain [
  bgroup "isUndefined"  [ bench "Unfixed" $ whnf isFixed Unfixed
                        , bench "Fixed 0" $ whnf isFixed (Fixed (0 :: Int))
                        ],
  bgroup "isFixed"      [ bench "undefined"   $ whnf isUndefined undefined
                        , bench "0 (defined)" $ whnf isUndefined (0 :: Int)
                        ]
  ]

