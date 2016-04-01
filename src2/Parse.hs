module Parse where

import Language.C
import Language.C.Parse
import Language.C.Pretty

--  Parse
--    get function declarations
--    call Parse.Types to get input/return types
--    return CFunctionSource FunctionName [TemplatedCType] CType FunctionBody DependenciesString
module Parse where

import Parse.Types
import Language.C.Parse (parseC)
import Language.C.Data.InputStream (readInputStream)
import Language.C.Data.Position (initPos)
import Language.C.Syntax.AST

fromLeft  :: Either a b -> Maybe a
fromLeft  (Left  x) = Just x
fromLeft   _        = Nothing

fromRight :: Either a b -> Maybe b
fromRight (Right x) = Just x
fromRight  _        = Nothing

parse :: FilePath -> IO (Maybe CTranslUnit)
parse fileName = do
  inputStream <- readInputStream fileName
  return $ fromRight $ parseC inputStream fileName (initPos fileName)

debugParse :: FilePath -> IO (Maybe CTranslUnit)
debugParse fileName = do
  inputStream <- readInputStream fileName
  let result = parseC inputStream fileName (initPos fileName)
  putStrLn $ "Error (if any) from parsing is: " ++ show (fromLeft result)
  return $ fromRight result

fstCTranslUnit :: CTranslationUnit t -> [CExternalDeclaration t]
fstCTranslUnit (CTranslUnit x _) = x

-- Use something like `partition noPointerFunctional` to get lists of declarations to/to-not consider

-- print the code with additional newlines between declarations
-- parse name >>= (return . fstCTranslUnit) >>= mapM_ ((putStrLn "" >>) . print . pretty)

--  Parse.Types
--    extract return type
--    extract input types
--      generate input templates
module Parse.Types where

isFunctional :: CExternalDeclaration a -> Maybe (CFunctionDef b)

returnType :: CFunctionDef a -> CTypeSpecifier b | CDerivedDeclarator c |

inputTypes :: CFunctionDef a -> [_]

isPointer :: _ -> Bool

noPointerFunctional :: CExternalDeclaration a -> Bool
noPointerFunctional x = isJust $ isFunctional x && not . isPointer . returnType . fromJust $ isFunctional x && not . any isPointer . inputTypes $ isFunctional

--  Parse.Types.Templates
--    make CType -> String templates (e.g. CUInt -> String)
--    should be able to use Show, except for "longer" types


{-
  --  Language.C doesn't seem to have much that would be directly useful for this. Pushing off as unnecessary for now.
  --  Parse.Dependencies
  --    attempt to get function dependencies
-}


