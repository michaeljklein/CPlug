module Data.Text.Aux where

import qualified Data.Text as T (Text, cons, snoc, lines, pack, unlines, unwords, words)
import Language.C.Pretty (Pretty, pretty)

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

packUnlines :: [String] -> T.Text
packUnlines = T.unlines . map T.pack

packUnwords :: [String] -> T.Text
packUnwords = T.unwords . map T.pack

showt :: Show a => a -> T.Text
showt = T.pack . show

prettyShowt :: Pretty p => p -> T.Text
prettyShowt = T.pack . show . pretty


