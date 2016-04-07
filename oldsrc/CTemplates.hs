module CTemplates where

-- import qualified Data.Text as T
import Language.C
-- import qualified Data.Attoparsec.Text as A
-- import qualified Data.Attoparsec.Combinator as AC

fromRight (Right x) = x

parsed = let fl = "src/test.c" in do {is <- readInputStream fl; return $ fromRight $ flip parseC (initPos fl) is}

splitCT (CTranslUnit ctransunit nodeinfo) = (ctransunit, nodeinfo)







{-
-- | `CTemplate` consists of a list of `Header`'s, a list of `Declaration`'s, and an initialization `Declaration`.
data CTemplate = CTemplate [CHeader] [CDeclaration] CDeclaration deriving (Show, Eq)

-- | A `CHeader` is either a #define clause or an #include
data CHeader = CDefine T.Text | CInclude T.Text deriving (Show, Eq)

-- | A `Declaration` is either a `CVariable` or `CFunction` declaration
data CDeclaration = CFunction CType T.Text [CDeclaration] T.Text | CVariable CType T.Text T.Text deriving (Show, Eq)

-- -- | A `CFunction` consists of a return `CType`, a function name (string), a list of input `CVariable`'s, and the function body
-- data CFunction = CFunction CType T.Text [CVariable] T.Text

-- -- | A `CVariable` consists of a `CType`, a name (string), and its initialization (if any)
-- data CVariable = CVariable CType T.Text T.Text

-- Don't forget to clean up
data CType =      CVoid             |
                  CIntegral         |
                  CFloating         |
                  COther            |
                  Pointer CType     |
                  CArray Int CType  deriving (Show, Eq)

data CIntegral  = CChar       |
                  CUIntegral  |
                  CSIntegral  deriving (Show, Eq)

data CUIntegral = CUChar      |
                  CUShort     |
                  CUInt       |
                  CULong      |
                  CULLong     deriving (Show, Eq)

data CSIntegral = CSChar      |
                  CShort      |
                  CInt        |
                  CLong       |
                  CLLong      deriving (Show, Eq)

data CFloating =  CFloat      |
                  CDouble     deriving (Show, Eq)

-}



-- day :: Parser DayOfWeek
--   day = monday    <|>
--         tuesday   <|>
--         -- ...
--     where monday = A.stringCI "monday" *> pure Monday

-- cType :: Parser CType
-- cIntegral :: Parser CIntegral
-- cUIntegral :: Parser CUIntegral
-- cSIntegral :: Parser CSIntegral
-- cFloating :: Parser CFloating

-- timeUnit :: Parser TimeUnit
-- timeKeyword :: Parser TimeKeyword
-- day :: Parser DayOfWeek
-- time :: Parser TimeOfDay
-- date :: Parser Date


 -- CChar(..),    CSChar(..),   CUChar(..)
 --        , CShort(..),   CUShort(..),  CInt(..),      CUInt(..)
 --        , CLong(..),    CULong(..)
 --        , CPtrdiff(..), CSize(..),    CWchar(..),    CSigAtomic(..)
 --        , CLLong(..),   CULLong(..)
 --        , CIntPtr(..),  CUIntPtr(..), CIntMax(..),   CUIntMax(..)

-- stringChoices :: [Text] -> Parser Text
--     stringChoices = AC.choice . map A.stringCI

--     -- ...
--     where monday = stringChoices ["monday", "mon"] *> pure Monday

-- monday = A.stringCI "monday" *> pure Monday

-- stringPs s t = A.asciiCI (T.pack s) *> pure t

-- signed   =  stringPs "signed long long int"   CLLong  <|>
--             stringPs "signed long long"       CLLong  <|>
--             stringPs "signed long int"        CLong   <|>
--             stringPs "signed long"            CLong   <|>
--             stringPs "signed int"             CInt    <|>
--             stringPs "signed short int"       CShort  <|>
--             stringPs "signed short"           CShort  <|>
--             stringPs "signed char"            CSChar  <|>
--             stringPs "signed"                 CSInt

-- unsigned =  stringPs "unsigned long long int" CULLong <|>
--             stringPs "unsigned long long"     CULLong <|>
--             stringPs "unsigned long int"      CULong  <|>
--             stringPs "unsigned long"          CULong  <|>
--             stringPs "unsigned int"           CUInt   <|>
--             stringPs "unsigned short int"     CUShort <|>
--             stringPs "unsigned short"         CUShort <|>
--             stringPs "unsigned char"          CUChar  <|>
--             stringPs "unsigned"               CUInt

-- short     = stringPs "short int"              CShort  <|>
--             stirngPs "short"                  CShort

-- int       = stringPs "int"                    CInt

-- long      = stringPs "long long int"          CLLong  <|>
--             stringPs "long long"              CLLong  <|>
--             stringPs "long int"               CLong   <|>
--             stringPs "long"                   CLong

-- float     = stringPs "float"                  CFloat

-- double    = stringPs "double"                 CDouble

-- void      = stringPs "void"                   CVoid

-- cType = signed    <|>
--         unsgiend  <|>
--         short     <|>
--         int       <|>
--         long      <|>
--         float     <|>
--         double    <|>
--         void

-- pointerChar c  = c == '*' || c == ' '

-- getPointedness = A.takeWhile pointerChar

-- notTypeEndChar = A.inClass " *[]0123456789"

-- getArrayness   = A.takeWhile notTypeEndChar

-- infixl 4 <++>
-- f <++> g = (++) <$> f <*> g

-- -- Parse a balanced number of brackets
-- brackets :: Parser Text
-- brackets = A.asciiCI (T.pack "{") <++> (join <$> A.many brackets) <++> A.asciiCI (T.pack "}") <|> A.many1 (A.noneOf "{}")


-- cFunction = cType <|> getName <|> getArgs <|> getRest

-- getArgs = A.asciiCI (T.pack "(") *> A.manyTill getArg (A.asciiCI (T.Pack ")"))

-- getArg  = cType *> A.takeWhile (/= ',') *> A.skipWhile (\x -> x == ' ' || x == ',')

-- getRest = brackets


-- -- parse headers
-- -- drop comments
-- -- get declarations: type, name, (args), body

-- stripComments = A.skipMany comment

-- comment  = lineComment <|> blockComment

-- lineComment = A.asciiCI (T.pack "//") *> A.takeWhile (/= '\n') *> A.take 1

-- blockComment = A.asciiCI (T.pack "/*") *> A.manyTill anyChar (A.asciiCI (T.pack "*/")) *> A.take 2







-- char = A.stringCI "char" *> pure CChar
-- signed = char, short, short int, int, long long, long long int
-- unsigned
-- short
-- int
-- long
-- float
-- double
-- void

-- char

-- signed char

-- unsigned char

-- short
-- short int
-- signed short
-- signed short int

-- unsigned short
-- unsigned short int

-- int
-- signed
-- signed int

-- unsigned
-- unsigned int

-- long
-- long int
-- signed long
-- signed long int

-- unsigned long
-- unsigned long int

-- long long
-- long long int
-- signed long long
-- signed long long int

-- unsigned long long
-- unsigned long long int

-- float

-- double

-- long double

-- pointer, array types
