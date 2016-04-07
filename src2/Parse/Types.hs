module Parse.Types where

--  Parse.Types
--    extract return type
--    extract input types
--      generate input templates

isFunctional :: CExternalDeclaration a -> Maybe (CFunctionDef b)

returnType :: CFunctionDef a -> CTypeSpecifier b | CDerivedDeclarator c |

inputTypes :: CFunctionDef a -> [_]



