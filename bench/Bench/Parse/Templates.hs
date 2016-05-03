{-# LANGUAGE OverloadedStrings #-}

module Bench.Parse.Templates where

import Criterion.Main
import Data.Text (Text(..))
import Data.Typeable (typeOf, TypeRep(..))
import Foreign.C.Types

cTypeRep :: Text -> TypeRep
cTypeRep "char"                   = typeOf (undefined :: CChar)
cTypeRep "signed char"            = typeOf (undefined :: CSChar)
cTypeRep "unsigned char"          = typeOf (undefined :: CUChar)
cTypeRep "short"                  = typeOf (undefined :: CShort)
cTypeRep "short int"              = typeOf (undefined :: CShort)
cTypeRep "signed short"           = typeOf (undefined :: CShort)
cTypeRep "signed short int"       = typeOf (undefined :: CShort)
cTypeRep "unsigned short"         = typeOf (undefined :: CUShort)
cTypeRep "unsigned short int"     = typeOf (undefined :: CUShort)
cTypeRep "int"                    = typeOf (undefined :: CInt)
cTypeRep "signed"                 = typeOf (undefined :: CInt)
cTypeRep "signed int"             = typeOf (undefined :: CInt)
cTypeRep "unsigned"               = typeOf (undefined :: CUInt)
cTypeRep "unsigned int"           = typeOf (undefined :: CUInt)
cTypeRep "long"                   = typeOf (undefined :: CLong)
cTypeRep "long int"               = typeOf (undefined :: CLong)
cTypeRep "signed long"            = typeOf (undefined :: CLong)
cTypeRep "signed long int"        = typeOf (undefined :: CLong)
cTypeRep "unsigned long"          = typeOf (undefined :: CULong)
cTypeRep "unsigned long int"      = typeOf (undefined :: CULong)
cTypeRep "long long"              = typeOf (undefined :: CLLong)
cTypeRep "long long int"          = typeOf (undefined :: CLLong)
cTypeRep "signed long long"       = typeOf (undefined :: CLLong)
cTypeRep "signed long long int"   = typeOf (undefined :: CLLong)
cTypeRep "unsigned long long"     = typeOf (undefined :: CULLong)
cTypeRep "unsigned long long int" = typeOf (undefined :: CULLong)
cTypeRep "float"                  = typeOf (undefined :: CFloat)
cTypeRep "double"                 = typeOf (undefined :: CDouble)

benchParseTemplates :: [Benchmark]
benchParseTemplates = [
  bgroup "compute " [ bench "char"                   $ whnf cTypeRep "char"
                    , bench "signed char"            $ whnf cTypeRep "signed char"
                    , bench "unsigned char"          $ whnf cTypeRep "unsigned char"
                    , bench "short"                  $ whnf cTypeRep "short"
                    , bench "short int"              $ whnf cTypeRep "short int"
                    , bench "signed short"           $ whnf cTypeRep "signed short"
                    , bench "signed short int"       $ whnf cTypeRep "signed short int"
                    , bench "unsigned short"         $ whnf cTypeRep "unsigned short"
                    , bench "unsigned short int"     $ whnf cTypeRep "unsigned short int"
                    , bench "int"                    $ whnf cTypeRep "int"
                    , bench "signed"                 $ whnf cTypeRep "signed"
                    , bench "signed int"             $ whnf cTypeRep "signed int"
                    , bench "unsigned"               $ whnf cTypeRep "unsigned"
                    , bench "unsigned int"           $ whnf cTypeRep "unsigned int"
                    , bench "long"                   $ whnf cTypeRep "long"
                    , bench "long int"               $ whnf cTypeRep "long int"
                    , bench "signed long"            $ whnf cTypeRep "signed long"
                    , bench "signed long int"        $ whnf cTypeRep "signed long int"
                    , bench "unsigned long"          $ whnf cTypeRep "unsigned long"
                    , bench "unsigned long int"      $ whnf cTypeRep "unsigned long int"
                    , bench "long long"              $ whnf cTypeRep "long long"
                    , bench "long long int"          $ whnf cTypeRep "long long int"
                    , bench "signed long long"       $ whnf cTypeRep "signed long long"
                    , bench "signed long long int"   $ whnf cTypeRep "signed long long int"
                    , bench "unsigned long long"     $ whnf cTypeRep "unsigned long long"
                    , bench "unsigned long long int" $ whnf cTypeRep "unsigned long long int"
                    , bench "float"                  $ whnf cTypeRep "float"
                    , bench "double"                 $ whnf cTypeRep "double"
                    ],
  bgroup "equality" [ bench "char"                   $ whnf (\x -> cTypeRep "char"                  == cTypeRep x) "char"
                    , bench "signed char"            $ whnf (\x -> cTypeRep "signed char"           == cTypeRep x) "signed char"
                    , bench "unsigned char"          $ whnf (\x -> cTypeRep "unsigned char"         == cTypeRep x) "unsigned char"
                    , bench "short"                  $ whnf (\x -> cTypeRep "short"                 == cTypeRep x) "short"
                    , bench "short int"              $ whnf (\x -> cTypeRep "short int"             == cTypeRep x) "short int"
                    , bench "signed short"           $ whnf (\x -> cTypeRep "signed short"          == cTypeRep x) "signed short"
                    , bench "signed short int"       $ whnf (\x -> cTypeRep "signed short int"      == cTypeRep x) "signed short int"
                    , bench "unsigned short"         $ whnf (\x -> cTypeRep "unsigned short"        == cTypeRep x) "unsigned short"
                    , bench "unsigned short int"     $ whnf (\x -> cTypeRep "unsigned short int"    == cTypeRep x) "unsigned short int"
                    , bench "int"                    $ whnf (\x -> cTypeRep "int"                   == cTypeRep x) "int"
                    , bench "signed"                 $ whnf (\x -> cTypeRep "signed"                == cTypeRep x) "signed"
                    , bench "signed int"             $ whnf (\x -> cTypeRep "signed int"            == cTypeRep x) "signed int"
                    , bench "unsigned"               $ whnf (\x -> cTypeRep "unsigned"              == cTypeRep x) "unsigned"
                    , bench "unsigned int"           $ whnf (\x -> cTypeRep "unsigned int"          == cTypeRep x) "unsigned int"
                    , bench "long"                   $ whnf (\x -> cTypeRep "long"                  == cTypeRep x) "long"
                    , bench "long int"               $ whnf (\x -> cTypeRep "long int"              == cTypeRep x) "long int"
                    , bench "signed long"            $ whnf (\x -> cTypeRep "signed long"           == cTypeRep x) "signed long"
                    , bench "signed long int"        $ whnf (\x -> cTypeRep "signed long int"       == cTypeRep x) "signed long int"
                    , bench "unsigned long"          $ whnf (\x -> cTypeRep "unsigned long"         == cTypeRep x) "unsigned long"
                    , bench "unsigned long int"      $ whnf (\x -> cTypeRep "unsigned long int"     == cTypeRep x) "unsigned long int"
                    , bench "long long"              $ whnf (\x -> cTypeRep "long long"             == cTypeRep x) "long long"
                    , bench "long long int"          $ whnf (\x -> cTypeRep "long long int"         == cTypeRep x) "long long int"
                    , bench "signed long long"       $ whnf (\x -> cTypeRep "signed long long"      == cTypeRep x) "signed long long"
                    , bench "signed long long int"   $ whnf (\x -> cTypeRep "signed long long int"  == cTypeRep x) "signed long long int"
                    , bench "unsigned long long"     $ whnf (\x -> cTypeRep "unsigned long long"    == cTypeRep x) "unsigned long long"
                    , bench "unsigned long long int" $ whnf (\x -> cTypeRep "unsigned long long int"== cTypeRep x) "unsigned long long int"
                    , bench "float"                  $ whnf (\x -> cTypeRep "float"                 == cTypeRep x) "float"
                    , bench "double"                 $ whnf (\x -> cTypeRep "double"                == cTypeRep x) "double"
                    ]
  ]
