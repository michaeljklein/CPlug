module Parse.Templates where

--  Parse.Templates
--    make CType -> String templates (e.g. CUInt -> String)
--    should be able to use Show, except for "longer" types

import qualified Data.Text as T (Text)

-------------------------------------------------------------------------------------------------------------------------------------------------------

-- `CFunctionTemplate` consists of: function name, return type, list of input (type, var), function body
data CFunctionTemplate = CFunTempl {returnType   :: T.Text,
                                    functionName :: T.Text,
                                    inputVars    :: [(T.Text, T.Text)],
                                    functionBody :: T.Text
                                   } deriving (Eq, Ord, Show)

-- unTemplate :: CFunctionTemplate -> T.Text
-- unTemplate ct = T.unwords  [returnType ct,
--                             functionName ct,
--                             parens . T.concatMap (T.unwords . tupToList) . inputVars $ ct,
--                             functionBody ct]



