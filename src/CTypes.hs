{- LANGUAGE TemplateHaskell -}

module CTypes
    ( cUnsignedTypeNames,
      cSignedTypeNames,
      cFloatingTypeNames,
      cUnsignedTypeBoundsExpQ
    ) where

import Foreign.C.Types
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Data.Bits

-- for something to be marshallable to/from C, we need:
-- type constructor
-- ability to pass to/from C
-- bounds
-- signed/unsigned, array, pointer, struct (requires more complex parsing)


--  here's the outline for Haskell -> C -> Haskell
--  convert haskell types to C types, based on size, typeclass, instances, etc
--  make "megatype" out of all types passed to C, with associated access code for C, Haskell
--  make "megatpye" out of all types returned from C, with associated access code for C, Haskell
--  make C header and evaluation (main.c) code
--  make haskell module that :
--    has main function
--    converts haskell types to C types
--    generates C code
--    compiles C code
--    runs C code
--    returns result

--    a: unsure but should leave general, b: input type of CFuncion, c: output type of CFunction
--  generateCCode :: CTemplate a b c -> a -> CCode b c
--  compileCFunction :: CCode b c -> IO (CFunction b c)
--  runCFunction :: CFunction b c -> b -> IO c



-- These types are needed to accurately represent C function prototypes, in order to access C library interfaces in Haskell. The Haskell system is not required to represent those types exactly as C does, but the following guarantees are provided concerning a Haskell type CT representing a C type t:

-- If a C function prototype has t as an argument or result type, the use of CT in the corresponding position in a foreign declaration permits the Haskell program to access the full range of values encoded by the C type; and conversely, any Haskell value for CT has a valid representation in C.
-- sizeOf (undefined :: CT) will yield the same value as sizeof (t) in C.
-- alignment (undefined :: CT) matches the alignment constraint enforced by the C implementation for t.
-- The members peek and poke of the Storable class map all values of CT to the corresponding value of t and vice versa.
-- When an instance of Bounded is defined for CT, the values of minBound and maxBound coincide with t_MIN and t_MAX in C.
-- When an instance of Eq or Ord is defined for CT, the predicates defined by the type class implement the same relation as the corresponding predicate in C on t.
-- When an instance of Num, Read, Integral, Fractional, Floating, RealFrac, or RealFloat is defined for CT, the arithmetic operations defined by the type class implement the same function as the corresponding arithmetic operations (if available) in C on t.
--                                                                                                                                                                                                                                         When an instance of Bits is defined for CT, the bitwise operation defined by the type class implement the same function as the corresponding bitwise operation in C on t.



--  CSIntegral
--  CUIntegral
--  CFloating
--  CPointer
--  CStruct

--  COther = CFile | CFpos | CJumpBuf

--  SizableCType
--  sizes :: [Size]
--  sizeCType :: Size -> Maybe (Type a)
--  type requirements (e.g. bounded, signed, etc)

tupMap2 f (a,b) = (f a, f b)

typeBoundsExpQ :: String -> ExpQ
-- typeBoundsExpQ t = sigE [| (minBound, maxBound) |] (appT (appT (tupleT 2) (conT t)) (conT t))
typeBoundsExpQ t = appE (infixE (Just (varE (mkName "toInteger"))) (varE (mkName "***")) (Just (varE (mkName "toInteger")))) (sigE (tupE [varE (mkName "minBound"),varE (mkName "maxBound")]) (appT (appT (tupleT 2) (conT (mkName t))) (conT (mkName t))))

typesBoundsExpQ :: [String] -> ExpQ
typesBoundsExpQ = listE . map typeBoundsExpQ

-- AppE (InfixE (Just (VarE GHC.Real.toInteger)) (VarE Control.Arrow.***) (Just (VarE GHC.Real.toInteger))) (SigE (TupE [VarE GHC.Enum.minBound,VarE GHC.Enum.maxBound]) (AppT (AppT (TupleT 2) (ConT t)) (ConT t)))

cUnsignedTypeNames = ["CUChar","CUShort","CUInt","CULong","CULLong"]

cSignedTypeNames = ["CChar","CSChar","CShort","CInt","CLong","CWchar","CLLong"]

cFloatingTypeNames = ["CFloat","CDouble"]

cOtherTypeNames = ["CFile","CFpos","CJmpBuf","CSize","CSigAtomic","CPtrdiff","CIntMax","CUIntMax","CClock","CTime","CUSeconds","CSUSeconds","CIntPtr","CUIntPtr"]

cUnsignedTypeBoundsExpQ = typesBoundsExpQ cUnsignedTypeNames

cSignedTypeBoundsExpQ = typesBoundsExpQ cSignedTypeNames

cFloatingTypeBoundsExpQ = typesBoundsExpQ cFloatingTypeNames

data CUnsigned = IsCUChar Integer CUChar |  IsCUShort Integer CUShort |  IsCUInt Integer CUInt |  IsCULong Integer CULong |  IsCULLong Integer CULLong deriving (Eq, Ord, Show)

data CSigned = IsCChar Integer CChar |  IsCSChar Integer CSChar |  IsCShort Integer CShort |  IsCInt Integer CInt |  IsCLong Integer CLong |  IsCWchar Integer CWchar |  IsCLLong Integer CLLong deriving (Eq, Ord, Show)

data CFloating = IsCFloat Rational CFloat |  IsCDouble Rational CDouble deriving (Eq, Ord, Show)


class SizableCType a where
  -- sizedCTypes = [TypeQ]
  sizeCType :: (Ord b, Num b) => b -> Maybe a


-- data Fixable a = Fixed a | Blank

-- f Blank     Blank     = \x0 x1 -> f x0 x1
-- f Blank     (Fixed 3) = \x0    -> f x0 3
-- f (Fixed 2) (Fixed 3) =           f 2  3




-- instance SizableCType CUnsigned where
--   sizeCType = (($(cUnsignedTypeBoundsExpQ))!!)

-- instance SizableCType CSIntegral where


--  instance SizableCType CSIntegral where
--    sizeCType 8 = CSChar
--    ...

--  GenericCTemplate
--    specifyTemplate :: Type a -> Type b -> Maybe (CTemplate (Type a) (Type b))

--  SplittableTemplate : (eg abelian, left-assoc, right-assoc)
--    split function
--    apply function
--    join  function

--  CTemplate :
--    input type of resulting code
--    output type of resulting code
--    options passable to template
--    requirements of template

