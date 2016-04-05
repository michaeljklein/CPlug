module CGen.Typed () where

import Foreign.C.Types

class CShow a where
  showForC :: a -> T.Text


-- Skip most of this and just make a .h file with all the definitions plus unit tests
class Pipeable a where
  readPiped   :: CStringLen -> a
  writePiped  :: a -> CStringLen
  cPipeReader :: (T.Text, T.Text) -- Of form (name, code)
  cPipeWriter :: (T.Text, T.Text) -- Of form (name, code)


rawCCharReader :: [String]
rawCCharReader = ["void char_reader(FILE *ptr, char * buffer){",
                  "  buffer[0] = getc(ptr);                   ",
                  "}                                          "]

rawCCharWriter :: [String]
rawCCharWriter = ["int  char_writer(char * buffer, FILE *ptr){",
                  "  return putc(buffer[0], ptr);             ",
                  "}                                          "]

instance Pipeable CChar where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "char_reader", packUnlines rawCCharReader)
  cPipeWriter = (T.pack "char_writer", packUnlines rawCCharWriter)

instance Pipeable CChar where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CSChar where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CUChar where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CShort where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CUShort where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CInt where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CUInt where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CLong where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CULong where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CPtrdiff where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CSize where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CWchar where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CSigAtomic where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CLLong where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CULLong where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CIntPtr where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CUIntPtr where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CIntMax where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CUIntMax where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CClock where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CTime where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CUSeconds where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CSUSeconds where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CFloat where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

instance Pipeable CDouble where
  readPiped   =
  writePiped  =
  cPipeReader = (T.pack "", )
  cPipeWriter = (T.pack "", )

-- I don't think these can be sent...
-- instance Pipeable CFile where
--   readPiped   =
--   writePiped  =
--   cPipeReader = (T.pack "", )
--   cPipeWriter = (T.pack "", )

-- instance Pipeable CFpos where
--   readPiped   =
--   writePiped  =
--   cPipeReader = (T.pack "", )
--   cPipeWriter = (T.pack "", )

-- instance Pipeable CJmpBuf where
--   readPiped   =
--   writePiped  =
--   cPipeReader = (T.pack "", )
--   cPipeWriter = (T.pack "", )



          CChar,    CSChar,   CUChar
        , CShort,   CUShort,  CInt,      CUInt
        , CLong,    CULong
        , CPtrdiff, CSize,    CWchar,    CSigAtomic
        , CLLong,   CULLong
        , CIntPtr,  CUIntPtr, CIntMax,   CUIntMax
        , CClock,   CTime,    CUSeconds, CSUSeconds
        , CFloat,   CDouble
        , CFile,        CFpos,     CJmpBuf
        ) where

LLU
U
LL
