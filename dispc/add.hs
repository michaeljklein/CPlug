{-# LANGUAGE ForeignFunctionInterface #-}
module Main(main, c_add) where
import Foreign.C

foreign import ccall unsafe "test.h cadd" c_add :: CInt -> CInt -> CInt

-- foreign import ccall unsafe "math.h sin"
     -- c_sin :: CDouble -> CDouble

main = print $ c_add 2 3
