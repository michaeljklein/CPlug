module HSGen.Recompiler.Instances (classText, genInstance) where

import qualified Data.Text as T (Text, append, cons, empty, intercalate, lines,
                                pack, singleton, snoc, unlines, unwords, words)
import Data.Text.Aux (parens, showInt, unWordLines, wordLines)

-- | Example: `oneFix 13 = Text.pack "Fix a32"`
oneFix :: Int -> T.Text
oneFix = T.append (T.pack "Fix a") . showInt

-- | `fixes 3 == pack "Fix a3 -> Fix a2 -> Fix a1"`
fixes :: Int -> T.Text
fixes = T.intercalate (T.pack " -> ") . map oneFix . reverse . enumFromTo 1

-- | `makeFwr 2 == pack "Fix a2 -> Fix a1 -> Wrapped f r"`
makeFwr :: Int -> T.Text
makeFwr 0 = flip T.append (T.pack     "Wrapped f r") . fixes $ 0
makeFwr n = flip T.append (T.pack " -> Wrapped f r") . fixes $ n

-- | `makeFws 2 == pack "Fix a2 -> Fix a1 -> Wrapped g s"`
makeFws :: Int -> T.Text
makeFws 0 = flip T.append (T.pack     "Wrapped g s") . fixes $ 0
makeFws n = flip T.append (T.pack " -> Wrapped g s") . fixes $ n

-- | "Wrapped f r"
wr :: T.Text
wr = T.pack "Wrapped f r"

-- | "Wrapped g s"
ws :: T.Text
ws = T.pack "Wrapped g s"

makeXs :: Int -> T.Text
makeXs = T.unwords . map (T.cons 'x' . showInt) . enumFromTo 1

makeUnfixeds :: Int -> T.Text
makeUnfixeds = T.unwords . flip replicate (T.pack "Unfixed")

-- | `makeLambdaXs 3 == pack "\\x1 x2 x3 ->"`
makeLambdaXs :: Int -> T.Text
makeLambdaXs 0 = T.empty
makeLambdaXs n = flip T.append (T.pack " ->") . T.cons '\\' . makeXs $ n


-- | Generate the text for an instance of the class `Compilable` with `n`
--  arguments of the form `Fix ai`
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

-- | Raw text used by `classText`
classStr :: [String]
classStr = ["class Compilable a r b s | a -> r, b ->s, a s -> b, r b -> a where",
            "  resolve :: a -> r                                               ",
            "                                                                  ",
            "  ($$$) :: (r -> s) -> a -> b                                     ",
            "                                                                  ",
            "($$) :: Compilable a (a1 -> s) (Fix a1 -> t) s => a -> a1 -> t    ",
            "($$) f x = (($x) $$$ f) (Fixed x)                                 "]

-- | Text form of `Compilable`
classText :: T.Text
classText = T.pack . unlines $ classStr


