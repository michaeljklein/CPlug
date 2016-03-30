module Parse where

import Language.C
import Language.C.Parse
import Language.C.Pretty

--  Parse
--    get function declarations
--    call Parse.Types to get input/return types
--    return CFunctionSource FunctionName [TemplatedCType] CType FunctionBody DependenciesString

--  Parse.Types
--    extract return type
--    extract input types
--      generate input templates

--  Parse.Types.Templates
--    make CType -> String templates (e.g. CUInt -> String)

--  Parse.Dependencies
--    attempt to get function dependencies
