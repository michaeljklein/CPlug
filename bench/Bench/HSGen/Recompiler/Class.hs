{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Bench.HSGen.Recompiler.Class where

import Criterion.Main
import Data.Fixable
import Data.Undefined
import Data.Wrapped
import qualified HSGen.Recompiler.Class as C

class Resolvable a where
  type Resolution a
  resolve :: a -> Resolution a

instance Resolvable (Wrapped a b) where
  type Resolution (Wrapped a b) = Wrapped a b
  resolve w = w

instance Resolvable a => Resolvable (t -> a) where
  type Resolution (t -> a) = Resolution a
  resolve w = resolve $ w undefined


class FixResolvable a where
  type FixResolution a
  fixResolve :: a -> FixResolution a

instance FixResolvable (Wrapped a b) where
  type FixResolution (Wrapped a b) = Wrapped a b
  fixResolve w = w

instance FixResolvable a => FixResolvable (Fix t -> a) where
  type FixResolution (Fix t -> a) = FixResolution a
  fixResolve w = fixResolve $ w Unfixed


undef x = if isUndefined x then 1 else 0

t1 :: Int -> Wrapped (Int -> Int) Int
t1 x = defWrap $ undef x

t2 :: Int -> Int -> Wrapped (Int -> Int -> Int) Int
t2 x y = defWrap $ undef x + undef y

t3 :: Int -> Int -> Int -> Wrapped (Int -> Int -> Int -> Int) Int
t3 x y z = defWrap $ undef x + undef y + undef z


unfd Unfixed = 1
unfd _       = 0

f1 :: Fix Int -> Wrapped (Fix Int -> Int) Int
f1 x = defWrap $ unfd x

f2 :: Fix Int -> Fix Int -> Wrapped (Fix Int -> Fix Int -> Int) Int
f2 x y = defWrap $ unfd x + unfd y

f3 :: Fix Int -> Fix Int -> Fix Int -> Wrapped (Fix Int -> Fix Int -> Fix Int -> Int) Int
f3 x y z = defWrap $ unfd x + unfd y + unfd z

benchHSGenRecompilerClass :: IO ()
benchHSGenRecompilerClass = defaultMain [
  bgroup "control (id)" [ bench "id 0    "  $ whnf         id 0
                        ],

  bgroup "1st resolve " [ bench "resolve 1" $ whnf C.resolve t1
                        , bench "resolve 2" $ whnf C.resolve t2
                        , bench "resolve 3" $ whnf C.resolve t3
                        ],
  bgroup "1st resolve2" [ bench "resolve 1" $ whnf   resolve t1
                        , bench "resolve 2" $ whnf   resolve t2
                        , bench "resolve 3" $ whnf   resolve t3
                        ],
  bgroup "fix resolve " [ bench "resolve 1" $ whnf C.fixResolve f1
                        , bench "resolve 2" $ whnf C.fixResolve f2
                        , bench "resolve 3" $ whnf C.fixResolve f3
                        ],
  bgroup "fix resolve2" [ bench "resolve 1" $ whnf   fixResolve f1
                        , bench "resolve 2" $ whnf   fixResolve f2
                        , bench "resolve 3" $ whnf   fixResolve f3
                        ],
  bgroup "C apply     " [ bench "app 1    " $ whnf (t1 C.$$) 0
                        , bench "app 2    " $ whnf (t2 C.$$ 0 C.$$) 0
                        , bench "app 3    " $ whnf (t3 C.$$ 0 C.$$ 0 C.$$) 0
                        ],
  bgroup "C fix apply " [ bench "app 1    " $ whnf (f1 C.##) z
                        , bench "app 2    " $ whnf (f2 C.## z C.##) z
                        , bench "app 3    " $ whnf (f3 C.## z C.## z C.##) z
                        ]
  ]
    where
      z = Fixed 0

