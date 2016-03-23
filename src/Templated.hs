{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Templateds () where

import Data.Coerce

-- | `Fix` is essentially `Maybe` but renamed to 1) avoid confusion, 2) prevent "rogue" instances of `Maybe` from messing things up.
data Fix a = Fixed a | Unfixed

-- | Analogous to `isJust`
isFixed :: Fix a -> Bool
isFixed (Fixed x) = True
isFixed Unfixed = False

-- | Analogous to `isNothing
isUnfixed :: Fix a -> Bool
isUnfixed = not . isFixed

-- | (Unsafe) extraction of a value from `Fix`. Throws error if argument if `Unfixed`.
fromFixed (Fixed x) = x
fromFixed Unfixed = error "Applied `fromFixed` to `Unfixed`."

toMaybe :: Fix a -> Maybe a
toMaybe (Fixed x) = Just x
toMaybe  Unfixed  = Nothing

fromMaybe :: Maybe a -> Fix a
fromMaybe (Just x) = Fixed x
fromMaybe Nothing  = Unfixed

----------------------------------------------------------------------------------------------------

-- | This is rather self-explanatory. Biggest thing to not do is define an instance of the form:
--   `Constant (Fix a -> b)`, which will mess up the `Resolvable` class.
class Constant a
-- defaultConstant :: a

-- | I'm not entirely sure, but this alone may be entirely sufficient to define `Constant`, as long
--   as there are no instances like `Eq b => Eq (Maybe a -> b) where f == g = f Nothing == g Nothing` or
--   `SimpleFunction (a -> b)`.
--   Basically, functional equality is undecidable in general so a type(class)
--   with `Eq` is most likely constant.
-- instance Eq a => Constant a

-- | `Resolution` is exactly what one can get out of a `Resolvable`.
class Resolution a

-- instance Constant a => Resolution a

-- | A `ResolvableTo` with parameters `a` and `b` is a function that may be resolved to `b`.
class Resolution b => ResolvableTo a b where
  -- | `resolveTo` applies
  -- > Unfixed :: Constant a => Fixed a
  --   until not possible (i.e. a `Resolution` is reached).
  resolveTo :: a -> b

-- | This is the stopping condition for `Resolvable`.
instance Resolution a => ResolvableTo a a where
  resolveTo :: a -> a
  resolveTo = id

-- instance (a ~ b, Resolution a) => ResolvableTo a b where
--   resolveTo = id

-- | This is the recursive condition for `Resolvable`.
instance (Constant a, ResolvableTo b c) => ResolvableTo (Fix a -> b) c where
  resolveTo :: (Fix a -> b) -> c
  resolveTo f = resolveTo (f Unfixed)

instance (Resolution a, Resolution b) => Resolution (a -> b)

instance (ResolvableTo a b, ResolvableTo a c) => ResolvableTo a (b -> c) where
  resolveTo :: a -> (b -> c)
  resolveTo f x = resolveTo f
  -- resolveTo = resolveTo . resolveTo

-- instance (Constant a, ResolvableTo b (c -> d)) => ResolvableTo (Fix a -> b) (c -> d) where
--   resolveTo :: (Fix a -> b) -> (c -> d)
--   resolveTo f = resolveTo (f Unfixed)

class Resolvable a where
  resolve :: ResolvableTo a b => a -> b
  resolve = resolveTo
instance Constant a => Resolvable a
instance (Constant a, Resolvable b) => Resolvable (Fix a -> b)
----------------------------------------------------------------------------------------------------

-- | A `Boomerang` is, in a way, a generalization of a `Resolvable`. The main difference is that a `Resolvable`
--   returns a `Resolution` and a `Boomerang` applies a function to the `Resolution`, leaving the type otherwise untouched.
class (ResolvableTo a b, ResolvableTo a c) => BoomerangToWith a b c where
  -- | `boomerang f g` effectively applies `f` to the `Resolution` of `g`, leaving the rest of `g` untouched.
  boomerangToWith :: (b -> c) -> a -> a

  -- | `stick f g` effectively applies `f` to the `Resolution` of `g`, but replaces the rest of `g` with dummy
  --   variables (i.e. `g` is resolved, but still has type `Resolution r => Fix a0 -> .. -> r`.
  stickToWith     :: (b -> c) -> a -> a

instance Resolution a => BoomerangToWith a a a where
  boomerangToWith :: (a -> a) -> a -> a
  boomerangToWith f   =  f

  stickToWith     :: (a -> a) -> a -> a
  stickToWith     f   =  f

instance (Constant a, BoomerangToWith b c d) => BoomerangToWith (Fix a -> b) c d where
  boomerangToWith :: (c -> d) -> (Fix a -> b) -> Fix a -> b
  boomerangToWith f x y = boomerangToWith f (x y      )

  stickToWith     :: (c -> d) -> (Fix a -> b) -> Fix a -> b
  stickToWith     f x y = stickToWith f (x Unfixed)

instance (Constant b, BoomerangToWith a b c) => BoomerangToWith a (b -> c) c


class BoomerangToWith a b b => BoomerangTo a b where
  boomerangTo :: (b -> b) -> a -> a
  boomerangTo = boomerangToWith
  stickTo     :: (b -> b) -> a -> a
  stickTo     =     stickToWith

class Boomerang a where
  boomerang :: BoomerangTo a b => (b -> b) -> a -> a
  boomerang = boomerangTo
  stick     :: BoomerangTo a b => (b -> b) -> a -> a
  stick     = stickTo

----------------------------------------------------------------------------------------------------


 -- ($) :: (a -> b) -> a -> b

-- Now, need a class with application of functions, application of arguments, resolution, and....compilation

type BoomerangWithTo b c a = BoomerangToWith a b c

-- ($$) :: (Constant b, BoomerangToWith a (b -> c) c) => a -> b -> a
($$) p x = boomerangToWith ($ x) p

(C a, C b) => P (Fix a) (a -> b) where
  rs :: (Fix a -> (a -> b)) -> (a -> b)
  rs p = p Unfixed
  ap :: (Fix a -> (a -> b)) -> a -> b
  ap p x = p (Fixed x) x
  fm :: (Fix a -> (a -> b)) -> (c -> a) -> (Fix c -> (c -> b))
  fm p f fa a = p (liftF f fa) (f a)
  -- fe :: (Fix a -> (a -> b)) -> (b -> c) -> (Fix a -> (a -> c))
  -- fe p f = f . p

(C a, C b, C c) => P (Fix a -> Fix b) (a -> b -> c) where
  rs :: (Fix a -> Fix b -> (a -> b -> c)) -> (a -> b -> c)
  rs p = (p Unfixed) Unfixed
  ap :: (Fix a -> Fix b -> (a -> b -> c)) -> a -> (Fix b -> (b -> c))
  ap p x = \y -> p (Fixed x) (Fixed y) x y
  fm :: (Fix a -> Fix b -> (a -> b -> c)) -> (d -> a) -> (Fix d -> Fix b -> (d -> b -> c))
  fm p f fa fb a b = p (liftF f fa) fb b (f a)

data Nat = Zero | Succ Nat

toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n-1))

fromNat :: Nat -> Int
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n

testNat :: Int -> Bool
testNat n | n < 0 = True
          | True  = n == fromNat . toNat n

class Partial α β n where
  resolveP  ::                                    (α -> β) -> (β -> γ) -> (α -> γ)  -- Boomerang function, traverses to `β` and applies `(β -> γ)`, leaving the rest untouched
  resolveF  ::                                    (α -> β) ->  β                    -- β replace `Constant a => Fix a` with `Unfixed`
  resolveC  ::  Constant Ω =>                     (α -> β) ->  Ω                    -- Resolves `(α -> β)` to a constant
  ($$)      :: (Constant a, Partial δ ε (n-1)) => (α -> β) ->        a -> (δ -> ε)  -- Apply `c` to `(α -> β)` at argument 0, unless not of type `c` (fails silently)
  ($#)      :: (Constant b, Partial δ ε (n-1)) => (α -> β) -> Int -> b -> (δ -> ε)  -- Apply `c` to `(α -> β)` at argument `Int`, unless out of bounds or not of type `c` (fails silently)
  arity     ::  Int                                                                 -- Return the `arity` of `Partial α β n`

-- unResolve :: (α -> β) ->          -- Match arities of `α` and `β` (where matched means `α` has one less argument)

0:                                                             (a)
1:                         (fb)     ->                    (b ->(a))
2:                   (fc ->(fb))    ->               (c ->(b ->(a)))
3:             (fd ->(fc ->(fb)))   ->          (d ->(c ->(b ->(a))))
4:       (fe ->(fd ->(fc ->(fb))))  ->     (e ->(d ->(c ->(b ->(a)))))
5: (ff ->(fe ->(fd ->(fc ->(fb))))) ->(f ->(e ->(d ->(c ->(b ->(a))))))


0: (        ) -> (a      )
1: (fb      ) -> (b -> u0)
2: (fc -> f1) -> (c -> u1)
3: (fd -> f2) -> (d -> u2)
4: (fe -> f3) -> (e -> u3)
5: (ff -> f4) -> (f -> u4)

(a -> b -> c)      -> d -> c
(a -> b -> c -> d) -> e -> d
(a -> b -> c) -> (c -> d) -> (a -> b -> d)
(d -> a) -> (a -> b -> c) -> (d -> b -> c)
(a -> b -> c -> d) -> b -> a -> c -> d
(a -> b -> c -> d) -> a -> b -> c -> d

(a -> b) -> (b -> c) -> a -> c
(a -> b -> c) -> b -> a -> c
(a -> b) -> a -> b

uncurry :: (a -> b -> c) -> (a, b) -> c
fmap :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(.) :: (b -> c) -> (a -> b) -> a -> c
flip :: (a -> b -> c) -> b -> a -> c
($) :: (a -> b) -> a -> b



(C a, P b c) => P (Fix a -> b) (a -> c)




-- class PartialTo a b where
--   ($$) :: BoomerangToWith a (b -> c) c => a -> b -> a
--   ($$) p x = boomerangToWith ($ x) p
--   pmap :: BoomerangToWith a b c => (b -> c) -> a -> a
--   pmap = boomerangToWith



-- class Resolution b => Resolvable a b where
--   resolve :: a -> b

-- class Resolvable a b => Boomerang a b c where
--   boomerang :: (b -> c) -> a -> a
--   stick     :: (b -> c) -> a -> a

-- -- fmap :: (a -> b) -> f a -> f b
-- -- fmap id  ==  id
-- -- fmap (f . g)  ==  fmap f . fmap g
-- instance Functor (Resolvable a b) where
--   fmap ::

-- instance Functor (Boomerang  a b) where
--   fmap ::

-- -- embed pure expressions (pure), and
-- -- sequence computations and combine their results (<*>).
-- -- identity
-- --    pure id <*> v = v
-- -- composition
-- --    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- -- homomorphism
-- --    pure f <*> pure x = pure (f x)
-- -- interchange
-- --    u <*> pure y = pure ($ y) <*> u

-- -- fmap f x = pure f <*> x

-- -- If f is also a Monad, it should satisfy
-- --    pure = return
-- --    (<*>) = ap
-- instance Applicative (Resolvable a b) where
--   -- pure  ::
--   -- (<*>) ::

-- instance Applicative (Boomerang  a b) where
--   -- pure  ::
--   -- (<*>) ::

-- instance Monad (Resolvable a b) where
--   -- (>>=) ::

-- instance Monad (Boomerang  a b) where
--   -- (>>=) ::

-- instance Functor ((->) r) where
--     fmap = (.)
-- instance Applicative ((->) a) where
--     pure = const
--     (<*>) f g x = f x (g x)
-- instance Monad ((->) r) where
--     return = const
--     f >>= k = \ r -> k (f r) r

-- instance Functor [] where
--     fmap = map
-- instance Applicative [] where
--     pure x    = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]
--     xs *> ys  = [y | _ <- xs, y <- ys]
-- instance Monad []  where
--     xs >>= f             = [y | x <- xs, y <- f x]
--     (>>) = (*>)
--     return x            = [x]
--     fail _              = []

-- instance  Functor IO where
--    fmap f x = x >>= (return . f)
-- instance Applicative IO where
--     pure = return
--     (<*>) = ap
-- instance  Monad IO  where
--     m >> k    = m >>= \ _ -> k
--     return    = returnIO
--     (>>=)     = bindIO
--     fail s    = failIO s


-- Done: resolve  (with resolver  class)
--       ($)      (with boomerang class)
-- Need: compile

-- resolve :: (Fix a -> Fix b -> (a -> b -> c)) -> (a -> b -> c)

-- applyR :: a -> (Fix a -> Fix b -> (a -> b -> c)) -> (Fix b -> (b -> c))
-- applyR f a =     (unResolve       ((resolve f) a)) a

-- constR :: x -> (Fix a -> Fix b -> (a -> b -> c)) -> (Fix x -> Fix a -> Fix b -> (x -> a -> b -> c))
-- constR f = const (unResolve (const (resolve f)))

-- compile :: (Fix a -> Fix b -> (a -> b -> c)) -> (Fix a -> Fix b -> (a -> b -> c))

-- flipR   :: (Fix a -> Fix b -> (a -> b -> c)) -> (Fix b -> Fix a -> (b -> a -> c))
-- flipR  f = flip  (unResolve (flip  (resolve f)))

-- g $ f  =   g     (unResolve (g     (resolve f)))




{- The following is a prototype/toy instance of Resolvable (R) and Boomerang (B)

class R a where
    res :: a -> Int

instance R Int where
    res = id

instance R a => R (Fix Int -> a) where
    res f = res (f Unfixed)

class B a b where
    bw :: (b -> b) -> a -> a

instance B Int Int where
    bw :: (Int -> Int) -> Int -> Int
      bw f x = (f x)
      -- instance B a b => B (Fix Int -> a) b where
      --   bw :: (b -> b) -> (Fix Int -> a) -> (Fix Int -> a)
      --   bw f x = \y -> bw f (x Unfixed)

instance B a b => B (Fix Int -> a) b where
  --bw :: (b -> b) -> a -> a
    bw :: (b -> b) -> (Fix Int -> a) -> (Fix Int -> a)
      bw f x = \y -> bw f (x y)

ix = isFixed
nx = not . isFixed

tt :: Fix Int -> Fix Int -> Fix Int -> Int
tt x y z | nx x = (-1)
         | nx y = (-2)
         | nx z = (-3)
         | True = fromFixed x + fromFixed y + fromFixed z

-- Here are some POC examples:

-- > res $ tt
-- -1
-- > res $ tt Unfixed
-- -1
-- > res $ tt (Fixed 1)
-- -2
-- > res $ tt (Fixed 1) (Fixed 2)
-- -3
-- > res $ tt (Fixed 1) (Fixed 2) (Fixed 3)
-- 6

-}

-- data C a
-- instance C Int
-- instance C a => C [a]

-- a (a -> b)

-- a b (a -> b -> c)

-- a b c (a -> b -> c -> d)

-- (F a -> F b -> c)


-- class Resolvable b a where
--   resolve :: b -> a
--   fr      :: b -> (x -> a)

-- instance Resolvable b a => Resolvable (Fix c -> b) a where
--   resolve :: (Fix c -> b) -> a
--   resolve f = resolve (f (Unfixed))
--   -- fr :: c -> (Fix c -> b) -> a
--   fr    f = (\x -> fr (f (Fixed x)))


{-
resolve ::
resolve ::                     (Fix c -> b)   -> b
resolve ::           (Fix d -> (Fix c -> b))  -> b
resolve :: (Fix e -> (Fix d -> (Fix c -> b))) -> b

resolve :: (a -> b) -> b

fr :: v -> (a -> b) -> b

fr :: e -> d -> c ->           (Fix c -> b)   -> b
fr :: e -> d ->      (Fix d -> (Fix c -> b))  -> b
fr :: e -> (Fix e -> (Fix d -> (Fix c -> b))) -> b



Resolvable                               b

resolve ::                     (Fix c -> b)   -> b
resolve f = resolve (f Unfixed)
Resolvable                     (Fix c -> b)   -> b

resolve ::           (Fix d -> (Fix c -> b))  -> b
resolve f = resolve (f Unfixed)
Resolvable           (Fix d -> (Fix c -> b))

resolve :: (Fix e -> (Fix d -> (Fix c -> b))) -> b
resolve f = resolve (f Unfixed)
Resolvable (Fix e -> (Fix d -> (Fix c -> b)))

----------------------------------------------------------------------

Resolvable                               b

fr :: e -> d -> c ->           (Fix c -> b)   -> b
Resolvable                     (Fix c -> b)

fr :: e -> d ->      (Fix d -> (Fix c -> b))  -> b
Resolvable           (Fix d -> (Fix c -> b))

fr x f = fr (f (Fixed e))
fr :: e -> (Fix e -> (Fix d -> (Fix c -> b))) -> b
Resolvable (Fix e -> (Fix d -> (Fix c -> b)))


Resolvable                               b
Resolvable                     (Fix c -> b)
Resolvable           (Fix d -> (Fix c -> b))
Resolvable (Fix e -> (Fix d -> (Fix c -> b)))

-}



-- fixF :: (Fix a -> b) -> a -> b
-- fixF f x = f (Fixed x)

-- res  ::      (r a -> b) -> b
-- resF :: a -> (r a -> b)  -> b

-- res  ::           (r a -> r b -> c) -> c
-- resF :: a -> b -> (r a -> r b -> c) -> c


-- instance Constant a => ResolvableTo (Fix a) a a where
--     resolveTo      :: (Fix a -> a) -> a
--     resolveTo  f   = f  Unfixed
--     resolveToF     :: (Fix a -> a) -> a -> a
--     resolveToF f x = f (Fixed x)

-- instance Constant a => ResolvableTo (Fix a, Fix a) (a, a) a where
--     resolveTo  f     = f ( Unfixed ,  Unfixed )
--     resolveToF f (x,y) = f ((Fixed x), (Fixed y))

-- class Resolved c => ResolvableTo a c where
--     resolveTo  :: b -> c
--     resolveToF :: FunctionHead a b => a -> b -> c

-- instance Constant a => ResolvableTo a a where
--     resolveTo    f = f
--     resolveToF _ f = f



-- instance Constant a => ResolvableTo (Fix a -> a) a where
--     resolveTo    f = f Unfixed
--     -- resolveToF x f = f (Fixed x)


-- instance Resolved a => ResolvableTo a a where
--     resolveTo  :: a -> a
--     resolveToF :: ResolvableTo a b => a -> b -> a

--     resolveTo  f   = f
--     resolveToF f _ = f


-- instance Constant a => ResolvableTo a (Fix a) where
--     resolveTo  :: a -> Fix a
--     resolveTo = Fixed

--     resolveToF = const . Fixed


-- instance Constant a => ResolvableTo (Fix a -> a) a where
--     resolveTo  :: (Fix a -> a) -> a
--     resolveTo  = ($ Unfixed)

--     -- resolveToF :: ResolvableTo (Fix a -> a) b => (Fix a -> a) -> b -> a
--     resolveToF f x = f (Fixed x)

-- resolveTo  :: (Fix a -> b) -> b
-- resolveToF :: (Fix a -> b) -> a -> b
-- resolveTo  f   = f  Unfixed
-- resolveToF f x = f (Fixed x)

-- instance (Constant a, ResolvableTo b d) => ResolvableTo (Fix a -> b) d where
--     resolveTo :: (Fix a -> b) -> d
--     resolveTo f    = resolveTo  (f Unfixed)
--     resolveToF :: ResolvableTo (Fix a -> b) c => (Fix a -> b) -> c -> d
-- --    resolveTo  :: (Fix a -> b) -> c
-- --    ResolveToF :: ResolvableTo
--     resolveToF f x = resolveToF (f (Fixed x))

-- -- f          :: Fix a -> b
-- -- resolve f  :: b
-- -- resolveF f :: a -> b

-- -- g          :: Fix a -> Fix b -> c
-- -- resolve g  :: c
-- -- resolveF g :: a -> b -> c

-- -- resolveF g = \a ->       resolveF (g (Fixed a))
-- -- resolveF g = \a -> \b -> resolveF (g (Fixed a) (Fixed b))
-- -- resolveF g = \a -> \b ->           g (Fixed a) (Fixed b)





-- class (Resolvable a, Resolved b) => ResolvableTo a b where
--     resolveTo :: a -> b
--     resolveTo =  resolve

-- class (Resolvable a) => ResolvableWith a b where
--   resolveWith :: Resolved c => b -> a -> c
-- --resolve     :: Resolved d => a -> d

-- class ResolvableTo a c => ResolvableWithTo a b c where
--   resolveWith ::                b -> a -> c
-- --resolve     :: Resolved d =>       a -> d
-- --resolveTo   ::                     a -> c

-- class FResolvable a b where
--   fResolve :: Resolved


-- Compilable (a -> b) (Fix a -> (a -> b))

-- f = Compilable f' fCompiler
-- rawFunc  f  = f'
-- compiler f  = fCompiler
-- compile  f  = Compilable (resolve fCompiler) fCompiler


-- class Compilable c where
--   rawFunc   :: c (a -> b) -> a -> b
--   template  :: ResolvableTo r (c a) => c a -> r -> c a
--   compile   :: c a -> c a
--   compile   = resolveTo . template
--   pure      :: a -> c a
--   (<*>)       :: c (a -> b) -> c a -> c b


-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- identity
--   pure id <*> v = v
-- composition
--   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- homomorphism
--   pure f <*> pure x = pure (f x)
-- interchange
--   u <*> pure y = pure ($ y) <*> u

-- fmap id  ==  id
-- fmap (f . g)  ==  fmap f . fmap g
-- fmap :: (a -> b) -> f a -> f b



-- class PartialApp a b where
--   function :: a -> b




-- class Fixable a where

-- class Resolvable a => ResolvableOf a b where


-- class PartialApp a b where
--   rawFunc :: a -> b



-- -- fixVar :: a -> (Fix a -> b) -> b


-- class Fixable a where
--   fix ::

-- class PartialApp a b where
--   rawFunction :: a -> b

-- given: (a -> b) (Fix a -> (a -> b))
-- Want it to be instance of Compilable

-- (a -> b) (Fix a -> c)
-- (a -> b -> c) (Fix a -> Fix b -> d)

-- ((->) r)

-- class PartialApp a where



-- class Compilable a where
--   rawFunction :: a
--   resolvable  :: (Resolvable a, Resolved c) => a -> b














-- class Constant a

-- class Fixed   a

-- instance (Fixed a, Fixed b) => Fixed (a -> b)

-- -- instance Fixed (a -> b) => (Fixed a -> Fixed b)

-- class Unfixed a

-- instance (Unfixed a, Unfixed b) => Unfixed (a -> b)

-- class (Fixed a, Unfixed a) => Fixable a b where
--   fix   :: Fixed   a => a -> b
--   unfix :: Unfixed a => a -> b

-- instance (Fixable a b, Fixable b c) => Fixable (a -> b) (b -> c) where
--   -- fix :: Fixed (a -> b) => (a -> b) -> (b -> c)
--   -- unfix :: Unfixed


-- -- class (Fixed a, Unfixed a) => Fixable a where
-- --   fix   :: Fixed   b => a -> b
-- --   unfix :: Unfixed b => a -> b

-- -- instance (Fixable a, Fixable b) => Fixable (a -> b) where
-- --   -- fix   :: Fixed   c => (a -> b) -> c
-- --   -- unfix :: Unfixed c => (a -> b) -> c






-- Fix (a -> b) = Fix a -> Fix b
-- Fix (a -> b -> c) = Fix a -> Fix b -> Fix c

-- resolve :: (Resolvable a, Resolution b) => a -> b



























-- f :: Compilable (a -> b -> c) (Maybe a -> Maybe b -> (a -> b -> c)) Int
-- f :: Compilable (a -> b -> c) (Fix a -> Fix b -> (a -> b -> c)) Int
-- f :: Compilable (Fix a -> Fix b -> c) (Fix a -> Fix b -> (Fix a -> Fix b -> c)) Int

-- f :: C (a -> b -> c) (a -> b -> (a -> b -> c))



-- fromFixed :: Fix x -> x
-- fromFixed (Fixed x) = x

-- class Constant a

-- instance Constant Integer

-- instance Constant Int

-- class Fixable a b where
--   fix   :: (Fix a -> b) -> Fix a -> b
--   unfix :: (Fix a -> b) -> Fix a -> b
--   resolve :: (Fix a -> b) -> b

-- -- | Note that this is essentially (&&) (need `fix` and `Fixed x` for `f (Fixed x)`)
-- instance (Constant a, Constant b) => Fixable a b where
--   fix   f (Fixed x) = f (Fixed x)
--   fix   f  _        = f (Unfixed)
--   unfix f  _        = f (Unfixed)
--   resolve f         = f (Unfixed)
--   -- unfix f (Unfixed) = f (Unfixed)

-- instance (Constant a, Fixable b c) => Fixable a (b -> c) where
--   fix   f (Fixed x) = f (Fixed x)
--   fix   f  _        = f (Unfixed)
--   unfix f  _        = f (Unfixed)
--   resolve f         = (f (Unfixed))

-- class Fixable2 a b c where
--   fix2 :: (Fix a -> Fix b -> c) -> Fix a -> Fix b -> c
--   unfix2 :: (Fix a -> Fix b -> c) -> Fix a -> Fix b -> c
--   resolve2 :: (Fix a -> Fix b -> c) -> c

-- instance (Constant a, Constant b, Constant c) => Fixable2 a b c where
--   fix2 f (Fixed x) = f (Fixed x)
--   fix2 f   _       = f (Unfixed)
--   unfix2 f _       = f (Unfixed)
--   resolve2 f        = f (Unfixed) (Unfixed)

-- instance (Constant a, Fixable2 b c d) => Fixable2 a (b -> c) d where
--   -- fix2 :: (Fix a -> Fix (b -> c) -> d) -> Fix a -> Fix (b -> c) -> d
--   fix2 f (Fixed x) (Fixed y) = f (Fixed x) (Fixed y)
--   fix2 f (Fixed x)  _        = f (Fixed x) (Unfixed)
--   fix2 f  _        (Fixed y) = f (Unfixed) (Fixed y)
--   fix2 f  _         _        = f (Unfixed) (Unfixed)
--   unfix2 f _        _        = f (Unfixed) (Unfixed)
--   resolve2 f                  = f (Unfixed) (Unfixed)

-- class Fixable3 a b where
--   fix3 :: (Fix a -> b) -> Fix a -> b
--   resolve3 :: (Fix a -> b) -> b

-- instance (Constant a, Constant b) => Flexible a b where
--   fix3 (Fixed x) = f (Fixed x)
--   fix3   _       = f (Unfixed)
--   unfix3 _       = f (Unfixed)

-- instance (Flexible a c, Constant b) => Flexible (a -> b) c where
--   fix (Unfixed

-- class Compilable where
--   core
--   apply
--   flip
--   compile

-- instance Compilable CType

-- instance (CType x, Compilable r) => Compilable (x -> r)

-- class PrintfType r
-- printf :: PrintfType r => String -> r

-- instance PrintfType (IO ())

-- instance (PrintfArg x, PrintfType r) => PrintfType (x -> r)



-- Compilable (a -> b) (f a -> (a -> b)) (f a)
-- g = Compilable ((a -> b) -> c) (f (a -> b) -> ((a -> b) -> c)) (f (a -> b))
-- core g :: (a -> b) -> c
-- rec  g :: Fixable f => f (a -> b) -> ((a -> b) -> c)
-- res  g :: Fixable f => f (a -> b)
-- comp g = rec res
-- ap   g :: (a -> b -> c) -> a -> (b -> c)
-- ap'  g ::

-- Flippable fl => cFlip (Compilable (fl a) b) = Compilable (cFlip' (fl a)) b



-- data Compilable a b = Compilable { cr :: (a -> b), rc :: (a -> (a -> b))}

-- unfix (Fixed x) = x

-- class Rec a b where
--   core :: a
--   rec  :: a -> b
--   gRec :: b


-- -- f :: Compilable (a -> (b -> (c -> d))) (Fix a -> (Fix b -> Fix c) -> (a -> (b -> (c -> d)))
-- -- g :: Compilable (     (b -> (c -> d))) (         (Fix b -> Fix c) -> (     (b -> (c -> d)))
-- -- h :: Compilable (     (     (c -> d))) (         (         Fix c) -> (     (     (c -> d)))

-- Compilable (a -> b)      (Fix a -> (a -> b))
-- Compilable (a -> b -> c) (Fix (a -> b) -> (a -> b -> c))

-- Compilable ( a    -> b) (Fix  a    -> ( a    -> b))
-- Compilable ((a,b) -> c) (Fix (a,b) -> ((a,b) -> c))

-- Fix (a,b) == (Fix a, Fix b)

-- instance (Fixable a, Fixable b) => Fixable (a,b)

-- (Compilable (a -> b) (Fix a -> (a -> b))) => (Compilable (a -> b -> c) (Fix a -> Fix b -> (a -> b -> c)))

-- (Fixable f => f (a -> b)) == (Fixable f => f a -> f b)

-- instance (Fixable a, Fixable b) => Fixable (a -> b)


-- fix   (a $ b) = fix a   $ fix b
-- unfix (a $ b) = unfix a $ unfix b

-- foo :: FooType a => a
-- foo = bar (return ())

-- class FooType a where
--       bar :: IO () -> a

-- instance FooType (IO ()) where
--       bar = id

-- instance (Show x, FooType r) => FooType (x -> r) where
--       bar s x = bar (s >> print x)

-- > Dan
-- >
-- > Victor Nazarov wrote:
-- > >
-- > >> {-# OPTIONS -fglasgow-exts #-}
-- > >> {-# OPTIONS -fallow-undecidable-instances #-}
-- > >
-- > > data Zero
-- > > data Succ a
-- > >
-- > > class Nary n x y | n x -> y where
-- > >   nary :: n -> x -> [String] -> y
-- > >
-- > > instance Nary Zero x x where
-- > >   nary _ x [] = x
-- > >
-- > > instance (Nary n y z, Read x) => Nary (Succ n) (x->y) z where
-- > >   nary _ f (x:xs) = nary (undefined::n) (f $ read x) xs

-- class Fixable a where
--   finished :: a -> Bool
--   fixed :: a -> Bool
--   unfixed :: a -> Bool
--   -- fix :: (a -> b) -> b

-- instance Fixable (Fix a) where
--   finished _ = False
--   fixed (Fixed x) = True
--   fixed _ = False
--   unfixed = not.fixed
--   -- fix = id

-- instance Fixable (a -> a) where
--   finished _ = True
--   fixed _ = True
--   unfixed = not.fixed


-- data Compilable1 a b         = Compilable1 (Fix a -> Fix b)                   (Fix a -> (a -> b))

-- data Compilable2 a (b->c)    = Compilable2 (Fix a -> Fix b -> Fix c)          (Fix a -> Fix b -> (a -> b -> c))

-- data Compilable3 a (b->c->d) = Compilable3 (Fix a -> Fix b -> Fix c -> Fix d) (Fix a -> Fix b -> Fix c -> (a -> b -> c -> d))

-- data Compilable a b = Compilable {core :: (Fix a -> b),
--                                   rec  :: (Fix a -> (Fix a -> b))}

-- data Compilable2 a b c = Compilable2 {core2 :: (Fix a -> Fix b -> c), rec2 :: (Fix a -> Fix b -> (Fix a -> Fix b -> c))}

-- apply :: a -> Compilable a b -> c
-- apply cble x = Compilable {core = core cble (Fix x), rec =

-- class Recorded a b where
--   core :: a -> b
--   rec  :: a -> (a -> b)


-- class Compilable a b where
--   core  :: a -> b
--   rec   :: a -> (a -> b)
--   compile :: (a -> b) -> (a -> b)

-- class Fixable f where
--   fix :: Fix a -> f a -> b

-- instance Fixable ((->) r) where
--   fix = ($)




-- -- So, we have some sort of type with composition, application, take out, return (should be monad, but not completely sure..)
-- -- Additionally, we have 'recompile', which is idempotent
-- --
-- -- (.), ($), flip, extract, return, compile

-- f = Compilable f' (Nothing, (Nothing, Nothing)) compiler

-- f = Compilable (a -> (b -> (c -> d))) (Maybe a, (Maybe b, Maybe c)) ((Maybe a, (Maybe b, Maybe c)) -> (a -> (b -> (c -> d))))

-- f x = Compilable (f' x) (Just x, (Nothing, Nothing)) compiler

-- f x y = Compilable (f' x y) (Just x, (Just y, Nothing)) compiler

-- f      :: Compilable (a -> b -> c) (Maybe a, (Maybe b, d)) compiler

-- flip f :: Compilable (b -> a -> c) (Maybe b, (Maybe a, d)) (flip compiler)

-- f $ x  :: Compilable (     b -> c) (Maybe b, Fixes a)

-- f = f' (Blank a, Blank b) fCompiler
-- f :: Compilable (a -> b -> c) Application ((Application a, (Application b, Application End)) -> (a -> b -> c))
-- f x = (f' x) (Fixed x, Blank b) fCompiler
-- flip f = (flip f') (Blank b, Blank a) (flip fCompiler)
-- flip f y = (flip f' y) (Fixed y, Blank a) (flip fCompiler)

-- f' :: a -> b -> c
-- fCompiler :: ApplicationOf a -> ApplicationOf b -> (a -> b -> c)

-- {- | Instances of 'Monad' should satisfy the following laws:

-- * @'return' a '>>=' k  =  k a@
-- * @m '>>=' 'return'  =  m@
-- * @m '>>=' (\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@

-- Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:

-- * @'pure' = 'return'@
-- * @('<*>') = 'ap'@

-- The above laws imply:

-- * @'fmap' f xs  =  xs '>>=' 'return' . f@
-- * @('>>') = ('*>')@

-- and that 'pure' and ('<*>') satisfy the applicative functor laws.

-- The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
-- defined in the "Prelude" satisfy these laws.
-- -}
-- class Applicative m => Monad m where
--     -- | Sequentially compose two actions, passing any value produced
--     -- by the first as an argument to the second.
--     (>>=)       :: forall a b. m a -> (a -> m b) -> m b

--     -- | Sequentially compose two actions, discarding any value produced
--     -- by the first, like sequencing operators (such as the semicolon)
--     -- in imperative languages.
--     (>>)        :: forall a b. m a -> m b -> m b
--     m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
--     {-# INLINE (>>) #-}

--     -- | Inject a value into the monadic type.
--     return      :: a -> m a
--     return      = pure

--     -- | Fail with a message.  This operation is not part of the
--     -- mathematical definition of a monad, but is invoked on pattern-match
--     -- failure in a @do@ expression.
--     fail        :: String -> m a
--     fail s      = error s


-- data T a b = T (a -> b) (Maybe a -> (a -> b))

-- instance Functor T a b where
--   fmap (T a b)

-- Functor:
-- fmap id  ==  id
-- fmap (f . g)  ==  fmap f . fmap g
-- fmap :: (a -> b) -> f a -> f b


-- Applicative:

-- pure :: a -> f a
-- Lift a value.
-- (<*>) :: f (a -> b) -> f a -> f b
-- Sequential application.

-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u
-- pure = return
-- (<*>) = ap
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- Monad:
-- return a >>= k  =  k a
-- m >>= return  =  m
-- m >>= (x -> k x >>= h)  =  (m >>= k) >>= h
-- fmap f xs  =  xs >>= return . f

-- (>>=) :: forall a b. m a -> (a -> m b) -> m b
-- Sequentially compose two actions, passing any value produced by the first as an argument to the second
-- return :: a -> m a
-- Inject a value into the monadic type.



-- instance Functor ((,) a) where
--     fmap f (x,y) = (x, f y)


-- instance  Functor Maybe  where
--     fmap _ Nothing       = Nothing
--     fmap f (Just a)      = Just (f a)

-- instance Applicative Maybe where
--     pure = Just

--     Just f  <*> m       = fmap f m
--     Nothing <*> _m      = Nothing

--     Just _m1 *> m2      = m2
--     Nothing  *> _m2     = Nothing

-- instance  Monad Maybe  where
--     (Just x) >>= k      = k x
--     Nothing  >>= _      = Nothing

--     (>>) = (*>)

--     return              = Just
--     fail _              = Nothing

-- f :: Compilable (a -> b -> c) (Maybe a -> Maybe b -> Maybe c) Int
-- f = Compilable f' fCompiler 2

-- g :: Compilable (a -> b -> c) (Maybe a -> Maybe b -> Maybe c)




-- type Compilable r = Compilable ((->) r) ((->) ((->) r)) Int

-- instance Functor Compilable r where
--       fmap (Compilable f' fCompiler fCount) (Compilable g' gCompiler gCount) = Compilable (f' . g') (

-- instance Functor ((->) r) where
--       fmap = (.)

-- instance Applicative ((->) a) where
--       pure = const
--       (<*>) f g x = f x (g x)

-- instance Monad ((->) r) where
--       return = const
--       f >>= k = \ r -> k (f r) r


-- f       = f' fCompiler 2
-- f      :: Compilable (a -> b -> c) (Application a -> Application b -> (a -> b -> c))
-- f x     = (f' x) (fCompiler (Fixed x)) 1
-- f x    :: Compilable (b -> c) (Application b -> Application c -> (b -> c))
-- flip f  = (flip f') (flip fCompiler) 2
-- flip f :: Compilable (b -> a -> c) (Application b -> Application a -> (b -> a -> c))

-- g = g' gCompiler 3
-- g :: Compilable (a -> b -> c -> d) (Application a -> Application b -> Application c -> (a -> b -> c -> d))
-- g x = (g' x) (gCompiler (Fixed x)) 2



-- f         :: Compilable (a -> b -> c -> d) Unapplied                                    (Blank a, (Blank b, (Blank c, End))) fCompiler
-- f x       :: Compilable (     b -> c -> d) Applied (Fixed a, End)                       (Blank b, (Blank c, End))            fCompiler
-- f x y     :: Compilable (          c -> d) Applied (Fixed a, (Fixed b, End))                      (Blank c, End)             fCompiler
-- f x y z   :: Compilable (               d) Applied (Fixed a, (Fixed b, (Fixed c, End))) AllApplied                           fCompiler
-- flip f    :: Compilable (b -> a -> c -> d) Unapplied                                    (Blank b, (Blank a, (Blank c, End))) fCompiler
-- flip (f x):: Compilable (     c -> b -> d) Applied (                                    (Blank c, (Blank b, End))            fCompiler



-- g :: Compilable (     b -> c -> d) (Blank b, (Blank c, End)) gCompiler
-- h :: Compilable (          c -> d) (Blank c, End) hCompiler

-- (1,(2,(3,End)))



-- f' a  b  c  = a + b + c
--   _0 _1 _2

-- f     = Compilable (a -> b -> c -> d) (Maybe a, Maybe b, Maybe c) ((Maybe a, Maybe b, Maybe c) -> (a -> b -> c -> d))

-- f     = Compilable (a -> (b -> (c -> d))) (Maybe a, (Maybe b, (Maybe c, Maybe d)))

-- f     = Compilable (a -> b) (Maybe a) (Maybe a -> (a -> b))



-- f     = Compilable  f'      (_Int, _Int, _Int) Template

-- f 1   = Compilable (f' 1)   (1,    _Int, _Int) Template

-- f 1 2 = Compilable (f' 1 2) (1,   2    , _Int) Template

-- f 1 2 3 = f' 1 2 3


-- If f' is n-ary and ai are constant then f a1 a2 .. an = f' a1 a2 .. an

-- An ordinary function can be made compilable by taking Template = f





-- compile :: Compilable a => a -> a
-- compile f = if applied f
--                then compile' (coreFunc f) (appliedArgs f)

-- compile' :: Template t a => t a -> a -> a
-- compile' templ args =

-- fmap id  ==  id
-- fmap (f . g)  ==  fmap f . fmap g

-- A functor with application, providing operations to

-- embed pure expressions (pure), and
-- sequence computations and combine their results (<*>).
-- A minimal complete definition must include implementations of these functions satisfying the following laws:

-- identity
-- pure id <*> v = v
-- composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- homomorphism
-- pure f <*> pure x = pure (f x)
-- interchange
-- u <*> pure y = pure ($ y) <*> u
-- The other methods have the following default definitions, which may be overridden with equivalent specialized implementations:

-- u *> v = pure (const id) <*> u <*> v
-- u <* v = pure const <*> u <*> v
-- As a consequence of these laws, the Functor instance for f will satisfy

-- fmap f x = pure f <*> x
-- If f is also a Monad, it should satisfy

-- pure = return
-- (<*>) = ap

-- The Monad class defines the basic operations over a monad, a concept from a branch of mathematics known as category theory. From the perspective of a Haskell programmer, however, it is best to think of a monad as an abstract datatype of actions. Haskell's do expressions provide a convenient syntax for writing monadic expressions.

-- Instances of Monad should satisfy the following laws:

-- return a >>= k  =  k a
-- m >>= return  =  m
-- m >>= (x -> k x >>= h)  =  (m >>= k) >>= h
-- Furthermore, the Monad and Applicative operations should relate as follows:

-- pure = return
-- (<*>) = ap
-- The above laws imply:

-- fmap f xs  =  xs >>= return . f
-- (>>) = (*>)
-- and that pure and (<*>) satisfy the applicative functor laws.

-- The instances of Monad for lists, Maybe and IO defined in the Prelude satisfy these laws.

-- Minimal complete definition
-- (>>=)


-- instance Functor ((->) r) where
--       fmap = (.)

-- instance Applicative ((->) a) where
--       pure = const
--       (<*>) f g x = f x (g x)

-- instance Monad ((->) r) where
--       return = const
--       f >>= k = \ r -> k (f r) r
