module Data.Text.Extras where

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


