-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

import qualified Data.Text as T (Text, append, cons, empty, intercalate, lines,
                                pack, singleton, snoc, unlines, unwords, words)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

data Fix a = Unfixed | Fixed a deriving (Show, Eq, Ord)

isFixed :: Fix a -> Bool
isFixed (Fixed _) = True
isFixed        _  = False

fromFixed :: Fix a -> a
fromFixed (Fixed x) = x

data Wrapped a b = Wrap a b deriving (Show, Eq, Ord)

unwrapF :: Wrapped a b -> a
unwrapF (Wrap x _) = x

unwrapb :: Wrapped a b -> b
unwrapb (Wrap _ y) = y

-- | The `Compilable` class allows you to wrap a function in a
--  compiler/modifier. How? Suppose you have a function
--    > sum3 = \x y z -> x + y + z :: Num a => a -> a -> a -> a
--  and a method which simplifies `\x y z -> x + y + z` into
--  `\x y -> (x + y +)`, where `x + y` is evaluated exactly once instead of
--  every time the function is called. `Compilable` allows you to combine
--  these into a function:
--    > f' :: Num a -> Fix a -> Fix a -> Fix a -> Wrapped (a -> a -> a -> a) a
--  Where regular "fixed" arguments, such as `0`, are passed as `Fix 0` and "no
--  argument" is passed as `Unfixed`. When `f'` is evaluated, it returns a
--  `Wrapper` containing the function compiled, but with blank (`\ _ ->`)
--  arguments where `Fixed` were passed, and the result of evaluating the
--  function. This means that you can 1) use the function normally, but with
--  `Fixed` added to the arguments, 2) return a result of the return type
--  without applying all of the arguments, 3) utilize laziness to only compile
--  when the function from the `Wrapper` is called. Here's an example:
--  @
--  sum3 :: Fix Int -> Fix Int -> Fix Int -> Wrapper (Int -> Int -> Int -> Int) Int
--  sum3 x y z | isFixed x && isFixed y = Wrap (\__c -> (fromFixed x + fromFixed y) + c) undefined
--             | otherwise              = Wrap (\a b c -> a + b + c) (sum . map fromFixed $ [x,y,z])
--  @
--  Although not all cases are covered, and supposing that the
--  `(fromFixed x + fromFixed y)` is evaluated only once, `sum3 1 2` should be
--  equivalent to `(+ 3)`, as far as evaluation. (Although the poper order of
--  operations can be hard to ensure, the intended use case includes `IO` so this
--  is less of a concern).
--
--  The depencency `a b -> c` means that if we have `instance1 a b c1` and
--  `instance2 a b c2` then `c1 ~ c2`.
--
--  @
--    a ~ (Fix x0 -> Fix x1 -> .. -> b)
--    c ~ (Fix x0 -> Fix x1 -> .. -> d)
--  @
--  So a d -> c, c b -> a
--  To check that we have everything, lets write out all the possibilities:
--  @
--    a -> b, a -/> c, a -/> d, b -/> a, b -/> c, b -/> d, c -/> a, c -/> b, c -> d,
--    d -/> a, d -/> b, d -/> c
--    a b -/> c, a b -/> d, a c -> b, a c -> d, a d -> b, a d -> c
--    b c -> a, b c -> d, b d -/> a, b d -/> c, c d -/> a, c d -/> b
--    a b c -> d, a b d -> c, a c b -> d, a c d -> b, b c d -> a
--  @
--  Now we cross out all the non-dependencies:
--  @
--    a -> b, c -> d
--    a c -> b, a c -> d, a d -> b, a d -> c
--    b c -> a, b c -> d
--    a b c -> d, a b d -> c, a c b -> d, a c d -> b, b c d -> a
--  @
--  Now we cross out all dependencies which are a weaker form of another (for
--  example, cross out `a b c -> d` because we have the much stronger `c -> d`):
--  @
--    a -> b, c -> d
--    a d -> c
--    b c -> a
--  @
--  Much better. Now we can consolidate to the following class declaration:
--  (`classText` gives the text for this class.)
class Compilable a r b s | a -> r, b ->s, a s -> b, r b -> a where
  -- | `resolve` has the effective type of
  --  `(Fix a0 -> .. -> Fix ai -> r) -> r`, which means it basically applies
  -- its input to `Unfixed` until it has type `Wrapped _ _`.
  resolve :: a -> r

  -- | `($$$) f wrappedFunction` essentially applies `f` directly to the
  --  `Wrapped` part of `wrappedFunction`.
  ($$$) :: (r -> s) -> a -> b

-- | `($$)` is analogous to `($)`, except that it takes a `Compilable`,
--  wrapped function.
($$) :: Compilable a (a1 -> s) (Fix a1 -> t) s => a -> a1 -> t
($$) f x = (($x) $$$ f) (Fixed x)

-- | `genInstance 0` returns the text for this instance
instance Compilable (Wrapped f r) (Wrapped f r) (Wrapped g s) (Wrapped g s) where
  resolve :: Wrapped f r -> Wrapped f r
  resolve w = w
  ($$$) :: (Wrapped f r -> Wrapped g s) -> Wrapped f r -> Wrapped g s
  ($$$) h w = h (w )

-- | `genInstance 1` returns the text for this instance
instance Compilable (Fix a1 -> Wrapped f r) (Wrapped f r) (Fix a1 -> Wrapped g s) (Wrapped g s) where
  resolve :: (Fix a1 -> Wrapped f r) -> Wrapped f r
  resolve w = w Unfixed
  ($$$) :: (Wrapped f r -> Wrapped g s) -> (Fix a1 -> Wrapped f r) -> (Fix a1 -> Wrapped g s)
  ($$$) h w = \x1 -> h (w x1)

-- | `genInstance 2` returns the text for this instance
instance Compilable (Fix a2 -> Fix a1 -> Wrapped f r) (Wrapped f r) (Fix a2 -> Fix a1 -> Wrapped g s) (Wrapped g s) where
  resolve :: (Fix a2 -> Fix a1 -> Wrapped f r) -> Wrapped f r
  resolve w = w Unfixed Unfixed
  ($$$) :: (Wrapped f r -> Wrapped g s) -> (Fix a2 -> Fix a1 -> Wrapped f r) -> (Fix a2 -> Fix a1 -> Wrapped g s)
  ($$$) h w = \x1 x2 -> h (w x1 x2)



showInt :: Int -> T.Text
showInt = toStrict . toLazyText . decimal

-- | Add parentheses around a `Text` object
parens :: T.Text -> T.Text
parens = (T.cons '(') . (flip T.snoc ')')

-- | Example: `oneFix 13 = Text.pack "Fix a32"`
oneFix :: Int -> T.Text
oneFix = T.append (T.pack "Fix a") . showInt

fixes :: Int -> T.Text
fixes = T.intercalate (T.pack " -> ") . map oneFix . reverse . enumFromTo 1

makeFwr :: Int -> T.Text
makeFwr 0 = flip T.append (T.pack     "Wrapped f r") . fixes $ 0
makeFwr n = flip T.append (T.pack " -> Wrapped f r") . fixes $ n

makeFws :: Int -> T.Text
makeFws 0 = flip T.append (T.pack     "Wrapped g s") . fixes $ 0
makeFws n = flip T.append (T.pack " -> Wrapped g s") . fixes $ n

wr :: T.Text
wr = T.pack "Wrapped f r"

ws :: T.Text
ws = T.pack "Wrapped g s"

makeUnfixeds :: Int -> T.Text
makeUnfixeds = T.unwords . map T.pack . flip replicate "Unfixed"

makeXs :: Int -> T.Text
makeXs = T.unwords . map ((T.cons 'x') . showInt) . enumFromTo 1

makeLambdaXs :: Int -> T.Text
makeLambdaXs 0 = T.empty
makeLambdaXs n = flip T.append (T.pack " ->") . T.cons '\\' . makeXs $ n

-- | `wordLines` converts a Text input to a list of lists, where `wordLines text !! i !! j` is the jth word of the ith line of `text`.
wordLines   :: T.Text -> [[T.Text]]
wordLines   = map T.words . T.lines

unWordLines :: [[T.Text]] -> T.Text
unWordLines = T.unlines . map T.unwords

-- | Generate the text for an instance of the class `Compilable` with `n` arguments of the form `Fix ai`
genInstance :: Int -> T.Text
genInstance n = unWordLines  [[T.pack "instance Compilable", parens fwr, parens wr, parens fws, parens ws, T.pack "where"],
                              [T.pack "  resolve ::", parens fwr, T.pack "->", wr],
                              [T.pack "  resolve w = w", unfixeds],
                              [T.pack "  ($$$) ::", parens . T.unwords $ [wr, T.pack "->", ws], T.pack "->", parens fwr, T.pack "->", parens fws],
                              [T.pack "  ($$$) h w =", lambdaXs, T.pack "h", parens . T.unwords $ [T.singleton 'w', xs]]]
  where
    fwr       = makeFwr      n
    fws       = makeFws      n
    unfixeds  = makeUnfixeds n
    xs        = makeXs       n
    lambdaXs  = makeLambdaXs n

classStr :: [String]
classStr = ["class Compilable a r b s | a -> r, b ->s, a s -> b, r b -> a where",
            "  resolve :: a -> r                                               ",
            "                                                                  ",
            "  ($$$) :: (r -> s) -> a -> b                                     ",
            "                                                                  ",
            "($$) :: Compilable a (a1 -> s) (Fix a1 -> t) s => a -> a1 -> t    ",
            "($$) f x = (($x) $$$ f) (Fixed x)                                 "]

classText :: T.Text
classText = T.pack . map unlines $ classStr
