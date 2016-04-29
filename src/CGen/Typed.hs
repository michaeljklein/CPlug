module CGen.Typed where

import Foreign.C.Types
import qualified Data.Text as T (Text, snoc, pack)

-- | Adds 'U' to the end of a `Text` object
addUnsigned :: T.Text -> T.Text
addUnsigned = flip T.snoc 'U'

-- | Adds 'L' to the end of a `Text` object
addLong :: T.Text -> T.Text
addLong = flip T.snoc 'L'

showt :: Show a => a -> T.Text
showt = T.pack . show

-- | This class allows one to `show` objects from "Foreign.C.Types" in a way
-- that won't throw warnings if used in C code. For example,
-- 'unsigned long long' literals need the 'ULL' annotation.
class CShow a where
  showForC :: a -> T.Text

instance CShow CChar where
  showForC = showt

instance CShow CShort where
  showForC = showt

instance CShow CLong where
  showForC = addLong . showt

instance CShow CPtrdiff where
  showForC = showt

instance CShow CLLong where
  showForC = addLong . addLong . showt

instance CShow CIntPtr where
  showForC = showt

instance CShow CClock where
  showForC = showt

instance CShow CFloat where
  showForC = showt

instance CShow CSChar where
  showForC = showt

instance CShow CUShort where
  showForC = addUnsigned . showt

instance CShow CULong where
  showForC = addLong . addUnsigned . showt

instance CShow CSize where
  showForC = showt

instance CShow CULLong where
  showForC = addLong . addLong . addUnsigned . showt

instance CShow CUIntPtr where
  showForC = addUnsigned . showt

instance CShow CTime where
  showForC = showt

instance CShow CDouble where
  showForC = showt

instance CShow CUChar where
  showForC = addUnsigned . showt

instance CShow CInt where
  showForC = showt

instance CShow CWchar where
  showForC = showt

instance CShow CIntMax where
  showForC = showt

instance CShow CUSeconds where
  showForC = showt

instance CShow CUInt where
  showForC = addUnsigned . showt

instance CShow CSigAtomic where
  showForC = showt

instance CShow CUIntMax where
  showForC = showt

instance CShow CSUSeconds where
  showForC = showt

-- instance CShow CFile where
--   showForC = showt
-- instance CShow CFpos where
--   showForC = showt
-- instance CShow CJmpBuf where
--   showForC = showt

-- CChar
-- CShort
-- Long
-- CPtrdiff
-- CLLong
-- CIntPtr
-- CClock
-- CFloat
-- CFile
-- CSChar
-- CUShort
-- CULong
-- CSize
-- CULLong
-- CUIntPtr
-- CTime
-- CDouble
-- CFpos
-- CJmpBuf
-- CUChar
-- CInt
-- CWchar
-- CIntMax
-- CUSeconds
-- CUInt
-- CSigAtomic
-- CUIntMax
-- CSUSeconds
