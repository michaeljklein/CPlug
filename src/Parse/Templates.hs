{-# LANGUAGE OverloadedStrings #-}


module Parse.Templates where

import Aux (tupToList)
import qualified Data.Text as T
import CGen.Typed (CShow(..), showForC)
import Control.Applicative ((<|>))
import Control.Monad (liftM, mfilter)
import Data.Text.Aux (parens)
import Data.Tuple (swap)
import Data.Typeable (typeOf, Typeable(..), TyCon(..), TypeRep(..), mkTyConApp, splitTyConApp)
import Foreign.C.Types
import Foreign.Ptr

--  Parse.Templates
--    make CType -> String templates (e.g. CUInt -> String)
--    should be able to use Show, except for "longer" types

import qualified Data.Text as T (Text, append, concat, intercalate, singleton, pack, unwords)

-------------------------------------------------------------------------------------------------------------------------------------------------------

-- `CFunctionTemplate` consists of: function name, return type, list of input (type, var), function body
data CFunctionTemplate = CFunTempl {returnType   :: T.Text,
                                    functionName :: T.Text,
                                    inputVars    :: [(T.Text, T.Text)],
                                    functionBody :: T.Text
                                   } deriving (Eq, Ord, Show)


unTemplate :: CFunctionTemplate -> T.Text
unTemplate ct = T.concat [ returnType ct
                         , T.singleton ' '
                         , functionName ct
                         , parens $ T.intercalate (T.pack ", ") $ map (T.unwords . tupToList) $ inputVars ct
                         , functionBody ct
                         ]

data NamedVal a = NamedV {valName  :: T.Text,
                          value    :: a
                         } deriving (Eq, Ord, Show)

-- update :: (a -> Maybe a) -> a -> a
-- update f x = let Just y = f x <|> Just x in y


updateInputVar :: CFunctionTemplate -> NamedVal a -> [(T.Text, T.Text)]
updateInputVar ct v = undefined

-- λ> insertAfterNewline "hi" "you say \n sometimes"
-- "you say hi\n sometimes"
insertAfterNewline :: T.Text -> T.Text -> T.Text
insertAfterNewline x y = before `T.append` x `T.append` after
  where
    (before, after) = T.span (/= '\n') y



namedVarDeclaration :: (CShow a, CTypeable a) => NamedVal a -> T.Text
namedVarDeclaration v = T.concat [ toCType  . value $ v
                                 , valName            v
                                 , "="
                                 , showForC . value $ v
                                 , ";\n"
                                 ]

declareNamedVar :: (CShow a, CTypeable a) => CFunctionTemplate -> NamedVal a -> T.Text
declareNamedVar ct v = namedVarDeclaration v `insertAfterNewline` functionBody ct

-- fixNamedVar :: CFunctionTemplate -> NamedVal a -> CFunctionTemplate
-- fixNamedVar ct v = ct {inputVars = updateInputVar ct v, functionBody = declareNamedVar ct v}


-- Requirements to fix a variable:
-- 1. variable name is in inputVars
-- 2. type input is the same type



-- CGen.Typed.showForC

update :: (a -> Maybe a) -> a -> a
update f x = let Just y = f x <|> Just x in y

-- updateWith ::

tt x y = mfilter (\z -> cTypeRep z == typeOf (value x)) . lookup (valName x) . map swap . inputVars $ y

-- fixVar :: CFunctionTemplate -> a -> CFunctionTemplate
-- fixVar ct x = if isJust varCType
--                  then if xTypeRep == cTypeRep (fromJust varCType)
--                          then SUBSTITUTE
--                          else Left ct
--                  else Left ct
--   where
--     xText      = showt  x
--     xTypeRep   = typeOf x
--     varCType = lookup xText . map swap . inputVars $ ct




-- cTypeRep :: T.Text -> TypeRep

-- typeOf (undefined :: ())       void
-- typeOf (undefined :: CPtrdiff) ptrdiff_t
-- typeOf (undefined :: CIntPtr)
-- typeOf (undefined :: CClock)
-- typeOf (undefined :: CFile)
-- typeOf (undefined :: CSize) size_t
-- typeOf (undefined :: CUIntPtr)
-- typeOf (undefined :: CTime)
-- typeOf (undefined :: CFpos)
-- typeOf (undefined :: CJmpBuf)
-- typeOf (undefined :: CWchar)
-- typeOf (undefined :: CIntMax)
-- typeOf (undefined :: CUSeconds)
-- typeOf (undefined :: CSigAtomic)
-- typeOf (undefined :: CUIntMax)
-- typeOf (undefined :: CSUSeconds)
--
-- Don't forget void, void *, etc. (IO (), Ptr ())


cTypeRep :: T.Text -> TypeRep
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

cTypeRep str  | T.last stripped == '*' = toPtrTypeRep . cTypeRep . T.strip . T.init $ stripped
              | T.last stripped == ']' = error $ "cTypeRep needs to support arrays with length: " ++ show stripped
              | otherwise             = error $ "cTypeRep does not support: " ++ show stripped
  where
    stripped = T.strip str

class Typeable a => CTypeable a where
  toCType :: a -> T.Text



-- λ> cTypeRep "char"
-- CChar
-- λ> cTypeRep "char *"
-- Ptr CChar
-- λ> cTypeRep "char **"
-- Ptr (Ptr CChar)
-- λ> cTypeRep "char ***"
-- Ptr (Ptr (Ptr CChar))
-- λ> cTypeRep "char   ** *"
-- Ptr (Ptr (Ptr CChar))


-- | Pseudocode illustrative example:
--
-- @
-- toPtrTypeRep Int == Ptr Int
-- @
toPtrTypeRep :: TypeRep -> TypeRep
toPtrTypeRep = applyTypeRep (undefined :: Ptr ())

-- | See `toPtrTypeRep`
toFunPtrTypeRep :: TypeRep -> TypeRep
toFunPtrTypeRep = applyTypeRep (undefined :: FunPtr ())

-- | Get the `TyCon` type of the first argument and apply it to the `TypeRep`
-- of the second
applyTypeRep :: Typeable a => a -> TypeRep -> TypeRep
applyTypeRep = typeRepCons . typeConHead

-- | Analogous to `(:)` for lists, where the element is a `TyCon` and the list
-- is a `TypeRep`
typeRepCons :: TyCon -> TypeRep -> TypeRep
typeRepCons = (. return) . mkTyConApp

-- | Get the `head` of a type, e.g.
--
-- >>> `typeRepHead (undefined :: IO Int)
-- IO
--
typeConHead :: Typeable a => a -> TyCon
typeConHead = fst . splitTyConApp . typeOf


-- long double
-- not currently supported by GHC


-- | fixVar is idempotent
-- fixVar :: NamedVar a -> CFunctionTemplate -> CFunctionTemplate
