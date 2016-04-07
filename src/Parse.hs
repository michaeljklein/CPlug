{-# LANGUAGE FlexibleContexts #-}
module Parse where

import Aux (apFst, lastTup, tupToList)
import Control.Monad (join, liftM, liftM2, liftM4, when)
import Data.Either.Aux (fromLeft, fromRight, mapLeft, unsafeFromLeft)
import Data.Maybe
import Data.Tuple (swap)
import qualified Data.Text as T (Text, pack, strip, unwords, words)
import Data.Text.Aux (prettyShowt)
import Language.C.Data.Ident
import Language.C.Parser (parseC)
import Language.C.Data.InputStream (readInputStream)
import Language.C.Data.Position (initPos)
import Language.C.Syntax.AST
import Language.C.Pretty
import System.IO.Unsafe (unsafePerformIO)
import Parse.Templates (CFunctionTemplate(..))

-------------------------------------------------------------------------------------------------------------------------------------------------------

parse :: FilePath -> IO (Maybe CTranslUnit)
parse = debugParse False

debugParse :: Bool -> FilePath -> IO (Maybe CTranslUnit)
debugParse debug filename = do
  inputStream <- readInputStream filename
  let position = initPos filename
  let parsed = parseC inputStream position
  when debug $ putStrLn $ "Error (if any) from parsing is: " ++ show (fromLeft parsed)
  return . fromRight $ parsed

parseFunctions :: FilePath -> IO [CFunctionTemplate]
parseFunctions = mkCFunTempls . liftM (liftM fstCTranslUnit) . parse

-- | (For debugging) Print the `CFunctionTemplate`s parsed from a file
printParsedFunctions :: FilePath -> IO ()
printParsedFunctions = join . liftM (mapM_ print) . parseFunctions

-------------------------------------------------------------------------------------------------------------------------------------------------------

fstCTranslUnit :: CTranslationUnit t -> [CExternalDeclaration t]
fstCTranslUnit (CTranslUnit x _) = x

isCFDefExt :: CExternalDeclaration t -> Bool
isCFDefExt (CFDefExt _) = True
isCFDefExt           _  = False

fromCFDefExt :: CExternalDeclaration t -> Maybe (CFunctionDef t)
fromCFDefExt (CFDefExt x) = Just x
fromCFDefExt           _  = Nothing

justFunctions :: [CExternalDeclaration t] -> [CFunctionDef t]
justFunctions = mapMaybe fromCFDefExt

eitherCFDefExt :: CExternalDeclaration t -> Either (CFunctionDef t) (CExternalDeclaration t)
eitherCFDefExt (CFDefExt x) = Left  x
eitherCFDefExt           x  = Right x

-------------------------------------------------------------------------------------------------------------------------------------------------------

-- | `CFunDef [CDeclarationSpecifier a] (CDeclarator a) [CDeclaration a] (CStatement a) a`
-- > CFunDef
-- > [CDeclarationSpecifier a] -- type specifier and qualifier
-- > (CDeclarator a)           -- declarator
-- > [CDeclaration a]          -- optional declaration list
-- > (CStatement a)            -- compound statement
--  a
fTypeSpec  :: CFunctionDef t -> [CDeclarationSpecifier t]
fTypeSpec  (CFunDef a _ _ _ _) = a

fDecl      :: CFunctionDef t -> CDeclarator t
fDecl      (CFunDef _ b _ _ _) =   b

fOptDecl   :: CFunctionDef t -> [CDeclaration t]
fOptDecl   (CFunDef _ _ c _ _) =     c

fStatement :: CFunctionDef t -> CStatement t
fStatement (CFunDef _ _ _ d _) =       d

fRest      :: CFunctionDef t -> t
fRest      (CFunDef _ _ _ _ e) =         e

-------------------------------------------------------------------------------------------------------------------------------------------------------

-- | `decIdent` and following are simple unpackers for a datatype from
--  Language.C that I wish used record syntax.
--  `CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] a`
decIdent :: CDeclarator t -> Maybe Ident
decIdent        (CDeclr a _ _ _ _) = a

decDerivedDecls :: CDeclarator t -> [CDerivedDeclarator t]
decDerivedDecls (CDeclr _ b _ _ _) =   b

decStrLit :: CDeclarator t -> Maybe (CStringLiteral t)
decStrLit       (CDeclr _ _ c _ _) =     c

decAttributes :: CDeclarator t -> [CAttribute t]
decAttributes   (CDeclr _ _ _ d _) =       d

decRest :: CDeclarator t -> t
decRest         (CDeclr _ _ _ _ e) =         e

derivedDeclaratorFunDecl  (CFunDeclr a _ _) = Just a
derivedDeclaratorFunDecl             _      = Nothing

derivedDeclaratorFunAttrs (CFunDeclr _ b _) =        Just b
derivedDeclaratorFunAttrs            _      =        Nothing

derivedDeclaratorFunRest  (CFunDeclr _ _ c) =               Just c
derivedDeclaratorFunRest             _      =               Nothing

-------------------------------------------------------------------------------------------------------------------------------------------------------

unpackIdent :: Ident -> String
unpackIdent (Ident x _ _) = x

-------------------------------------------------------------------------------------------------------------------------------------------------------

-- | This converts a typed C variable to (type, name), e.g.
--  `"unsigned int x" -> Just ("unsigned int", "x")`.
--  (Returns nothing on empty string.)
splitTypedVar :: T.Text -> Maybe (T.Text, T.Text)
splitTypedVar = liftM (apFst T.unwords) . lastTup . T.words


getFunctionName :: CExternalDeclaration t -> Maybe Ident
getFunctionName = join . liftM (decIdent . fDecl) . fromLeft . eitherCFDefExt

getFunctionNameText :: CExternalDeclaration t -> Maybe T.Text
getFunctionNameText = liftM (T.pack . unpackIdent) . getFunctionName

getFunctionReturnType :: Pretty (CDeclarationSpecifier t) => CExternalDeclaration t -> Maybe [String]
getFunctionReturnType = liftM (map (show . pretty) . fTypeSpec) . fromLeft . eitherCFDefExt

getFunctionReturnTypeText :: Pretty (CDeclarationSpecifier t) => CExternalDeclaration t -> Maybe T.Text
getFunctionReturnTypeText = liftM (T.unwords . map T.pack) . getFunctionReturnType

getFunctionInputTypes :: CExternalDeclaration t -> Maybe [CDeclaration t]
getFunctionInputTypes = join . join . liftM (liftM (liftM fst . fromRight) . head . map derivedDeclaratorFunDecl . decDerivedDecls . fDecl) . fromLeft . eitherCFDefExt

getFunctionInputTypesText :: Pretty (CDeclaration t) => CExternalDeclaration t -> Maybe [(T.Text, T.Text)]
getFunctionInputTypesText = liftM (mapMaybe (splitTypedVar . prettyShowt)) . getFunctionInputTypes

getFunctionBody :: CExternalDeclaration t -> Maybe (CStatement t)
getFunctionBody = liftM fStatement . fromLeft . eitherCFDefExt

getFunctionBodyText :: Pretty (CStatement t) => CExternalDeclaration t -> Maybe T.Text
getFunctionBodyText = liftM (T.strip . prettyShowt) . getFunctionBody

mkCFunTempl :: (Pretty (CStatement t), Pretty (CDeclarationSpecifier t), Pretty (CDeclaration t)) => CExternalDeclaration t -> Maybe CFunctionTemplate
mkCFunTempl = liftM4 (liftM4 CFunTempl) getFunctionReturnTypeText getFunctionNameText getFunctionInputTypesText getFunctionBodyText

mkCFunTempls :: (Monad m, Pretty (CStatement t), Pretty (CDeclarationSpecifier t), Pretty (CDeclaration t)) => m (Maybe [CExternalDeclaration t]) -> m [CFunctionTemplate]
mkCFunTempls = liftM (join . maybeToList . liftM (mapMaybe mkCFunTempl))
