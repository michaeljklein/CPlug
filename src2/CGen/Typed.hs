module CGen.Typed where

import Foreign.C.Types
import qualified Data.Text as T (Text, snoc)
import Textshow (showt)

addUnsigned :: T.Text -> T.Text
addUnsigned = T.snoc 'U'

addLong :: T.Text -> T.Text
addLong = T.snoc 'L'

class CShow a where
  showForC :: a -> T.Text

instance CShow CChar where
  showForC = showt

instance CShow CShort where
  showForC = showt

instance CShow addLong where
  showForC = addLong . showt

instance CShow CPtrdiff where
  showForC = showt

instance CShow CLaddLong where
  showForC = addLong . addLong . showt

instance CShow CIntPtr where
  showForC = showt

instance CShow CClock where
  showForC = showt

instance CShow CFloat where
  showForC = showt

instance CShow CFile where
  showForC = showt

instance CShow CSChar where
  showForC = showt

instance CShow CUShort where
  showForC = addUnsigned . showt

instance CShow CUaddLong where
  showForC = addLong . addUnsigned . showt

instance CShow CSize where
  showForC = showt

instance CShow CULaddLong where
  showForC = addLong . addLong . addUnsigned . showt

instance CShow CUIntPtr where
  showForC = addUnsigned . showt

instance CShow CTime where
  showForC = showt

instance CShow CDouble where
  showForC = showt

instance CShow CFpos where
  showForC = showt

instance CShow CJmpBuf where
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
