module CGen where

--  CGen
--    makeTemplate :: CFunctionSource -> CFunctionTemplate

-- So we have a major refactoring to use dlopen()
-- Now we need just Caller and Runner modules

-- CGen.Caller
-- CGen.Runner

-- CGen.Compiler
--  make compiler piper (pipe out program -> gcc(?) -> pipe to dlopen)
--  make config taker



{- Read/Write implementation notes:
     Pointer input/output *IS NOW* supported.

data CType =      CVoid             |
                  CIntegral         |
                  CFloating         |
                  COther            |
                  Pointer CType     |
                  CArray Int CType  deriving (Show, Eq)

data CIntegral  = CChar       |
                  CUIntegral  |
                  CSIntegral  deriving (Show, Eq)

data CUIntegral = CUChar      |
                  CUShort     |
                  CUInt       |
                  CULong      |
                  CULLong     deriving (Show, Eq)

data CSIntegral = CSChar      |
                  CShort      |
                  CInt        |
                  CLong       |
                  CLLong      deriving (Show, Eq)

data CFloating =  CFloat      |
                  CDouble     deriving (Show, Eq)

-}
