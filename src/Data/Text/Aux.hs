module Data.Text.Aux where

import Control.Monad (liftM2)
import qualified Data.Text as T (Text, append, cons, intercalate, lines, pack, snoc, unlines, unwords, words)
import Language.C.Pretty (Pretty, pretty)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Text.PrettyPrint.HughesPJ.Ext
import TextShow (showt)

-- | `showInt` should be equivalent to `pack . show` for the `Int` type, except
--  a bit faster since it uses `Text` library functions
showInt :: Int -> T.Text
showInt = toStrict . toLazyText . decimal

-- | Example:
--
-- >>> textAp "Hello" "World"
-- "Hello (World)"
--
textAp :: String -> T.Text -> T.Text
textAp ap = T.append (flip T.snoc ' ' . T.pack $ ap) . parens

-- | Example:
--
-- >>> wrapText '|' "text"
-- "|text|"
--
wrapText :: Char -> T.Text -> T.Text
wrapText = liftM2 (.) T.cons (flip T.snoc)

-- | Example:
--
-- >>> addArrows . map T.pack ["Bool", "String", "Int"]
-- "Bool -> String -> Int"
--
addArrows :: [T.Text] -> T.Text
addArrows = T.intercalate (T.pack " -> ")

-- | Example:
--
-- >>> unwords2 (T.pack "Hello") (T.pack "World")
-- "Hello World"
--
unwords2 :: T.Text -> T.Text -> T.Text
unwords2 = appendAfter (T.pack " ")

-- | Example:
--
-- >>> appendAfter ", " "Hello" "World"
-- "Hello, World"
--
appendAfter :: T.Text -> T.Text -> T.Text -> T.Text
appendAfter = (T.append .) . T.append

-- | Add parentheses around a `Text` object
parens :: T.Text -> T.Text
parens = (T.cons '(') . (flip T.snoc ')')

-- | `wordLines` converts a Text input to a list of lists, where
--   `wordLines text !! i !! j` is the jth word of the ith line of `text`.
wordLines   :: T.Text -> [[T.Text]]
wordLines   = map T.words . T.lines

-- | See `wordLines`
unWordLines :: [[T.Text]] -> T.Text
unWordLines = T.unlines . map T.unwords

-- | Shortcut for
--
-- > unlines . map pack
--
packUnlines :: [String] -> T.Text
packUnlines = T.unlines . map T.pack

-- | Shortcut for
--
-- > unwords . map pack
--
packUnwords :: [String] -> T.Text
packUnwords = T.unwords . map T.pack

-- | Convert a pretty-printable object to `Text`
prettyShowt :: Pretty p => p -> T.Text
prettyShowt = showt . pretty


