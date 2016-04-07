{-# LANGUAGE FlexibleContexts #-}
module Parse where

-- import Language.C
-- import Language.C.Parser
-- import Language.C.Pretty

--  Parse
--    get function declarations
--    call Parse.Types to get input/return types
--    return CFunctionSource FunctionName [TemplatedCType] CType FunctionBody DependenciesString

-- import Parse.Types
-- import Parse.Templates
import Control.Monad (join, liftM, liftM2, liftM4)
import Data.Maybe
import Data.Tuple (swap)
import qualified Data.Text as T (Text, pack, strip, unwords, words)
import Language.C.Data.Ident
import Language.C.Parser (parseC)
import Language.C.Data.InputStream (readInputStream)
import Language.C.Data.Position (initPos)
import Language.C.Syntax.AST
import Language.C.Pretty
import System.IO.Unsafe (unsafePerformIO)
-- import TextShow (showt)

-- `CFunctionTemplate` consists of: function name, return type, list of input (type, var), function body
data CFunctionTemplate = CFunTempl {returnType   :: T.Text,
                                    functionName :: T.Text,
                                    inputVars    :: [(T.Text, T.Text)],
                                    functionBody :: T.Text
                                   } deriving (Eq, Ord, Show)

unsafeFromLeft :: Either a b -> a
unsafeFromLeft (Left x) = x
unsafeFromLeft _        = error "called unsafeFromLeft on (Right x)"

fromLeft  :: Either a b -> Maybe a
fromLeft  (Left  x) = Just x
fromLeft   _        = Nothing

fromRight :: Either a b -> Maybe b
fromRight (Right x) = Just x
fromRight  _        = Nothing

parse :: FilePath -> IO (Maybe CTranslUnit)
parse filename = do
  inputStream <- readInputStream filename
  let position = initPos filename
  let parsed = parseC inputStream position
  return . fromRight $ parsed

debugParse :: FilePath -> IO (Maybe CTranslUnit)
debugParse filename = do
  inputStream <- readInputStream filename
  let position = initPos filename
  let parsed = parseC inputStream position
  putStrLn $ "Error (if any) from parsing is: " ++ show (fromLeft parsed)
  return . fromRight $ parsed

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

eitherCFDefExt (CFDefExt x) = Left  x
eitherCFDefExt           x  = Right x

mapLeft :: (t -> a) -> [Either t b] -> [Either a b]
mapLeft _ [] = []
mapLeft f (x@(Left  y) : xs) = Left (f y) : mapLeft f xs
mapLeft f (x@(Right y) : xs) = Right   y  : mapLeft f xs

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

unpackIdent :: Ident -> String
unpackIdent (Ident x _ _) = x

-- | Illustrative example: `lastTup [1,2,3,4] == ([1,2,3], 4).
--  Used to parse input vars, ex: `lastTup ["unsigned", "int", "x"] == Just (["unsigned", "int"], "x")`.
lastTup :: [a] -> Maybe ([a], a)
lastTup [] = Nothing
lastTup x  = Just . liftM2 (,) init last $ x


mapFst :: (a1 -> a2) -> (a1, a) -> (a2, a)
mapFst f (a,b) = (f a, b)

-- | This converts a typed C variable to (type, name), e.g.
--  `"unsigned int x" -> Just ("unsigned int", "x")`.
--  (Returns nothing on empty string.)
splitTypedVar :: T.Text -> Maybe (T.Text, T.Text)
splitTypedVar = liftM (mapFst T.unwords) . lastTup . T.words



ex = fromJust $ unsafePerformIO $ parse "src2/cex/parse.c" >>= return . liftM fstCTranslUnit
ex2 = head . decDerivedDecls . fDecl . unsafeFromLeft . head . map eitherCFDefExt $ ex
ex3 = fst . fromJust . fromRight . fromJust . derivedDeclaratorFunDecl $ ex2

ex4 = map (liftM decDerivedDecls . liftM fDecl) . map fromLeft . map eitherCFDefExt $ ex

ex5 = map (map fromJust . filter isJust) . map fromJust . filter isJust . map (liftM (map (liftM fromRight))) . map (liftM (map derivedDeclaratorFunDecl)) $ ex4

-- Each element of this is the list of input types (key) (pretty gives text form, which I might just parse myself..)
ex6 = map head $ map (map fst . map fromJust . filter isJust) ex5

inputTypeEx = map (map (splitTypedVar . T.pack . show . pretty)) $ ex6

parsed = parse "src2/cex/parse.c" >>= return . liftM fstCTranslUnit

oneParsed = liftM (liftM head) parsed

nParsed n = (fmap . fmap) (!!n) $ parsed


showt :: Show a => a -> T.Text
showt = T.pack . show

prettyShowt :: Pretty p => p -> T.Text
prettyShowt = T.pack . show . pretty

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
getFunctionInputTypesText = liftM (catMaybes . map splitTypedVar) . liftM (map prettyShowt) . getFunctionInputTypes

getFunctionBody :: CExternalDeclaration t -> Maybe (CStatement t)
getFunctionBody = liftM fStatement . fromLeft . eitherCFDefExt

getFunctionBodyText :: Pretty (CStatement t) => CExternalDeclaration t -> Maybe T.Text
getFunctionBodyText = liftM (T.strip . prettyShowt) . getFunctionBody

mkCFunTempl :: (Pretty (CStatement t), Pretty (CDeclarationSpecifier t), Pretty (CDeclaration t)) => CExternalDeclaration t -> Maybe CFunctionTemplate
mkCFunTempl = liftM4 (liftM4 CFunTempl) getFunctionReturnTypeText getFunctionNameText getFunctionInputTypesText getFunctionBodyText

-- CFunTempl {functionName :: T.Text,
--  returnType   :: T.Text,
--  inputVars    :: [(T.Text, T.Text)],
--  functionBody :: T.Text
-- }


one = fromJust . unsafePerformIO $ oneParsed

-- print the code with additional newlines between declarations
-- parse name >>= (return . fstCTranslUnit) >>= mapM_ ((putStrLn "" >>) . print . pretty)



