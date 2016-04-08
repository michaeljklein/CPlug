module HSGen.FFI where

import Aux (apToLast)
import Control.Monad (liftM2)
import qualified Data.Text as T (Text, append, concat, cons, intercalate, pack, singleton, unwords)
import Data.Fixed (Fix(..), isFixed)
import Data.Text.Aux (addArrows, appendAfter, parens, showInt, textAp, unwords2, wrapText)

-- | This is the standard language pragma for Haskell FFI
pragma :: T.Text
pragma = T.pack "{-# LANGUAGE ForeignFunctionInterface #-}\n"

-- | This generates the import line for "Foreign.C.Types"
typesImport :: [T.Text] -> T.Text
typesImport typeList = T.concat [T.pack "import Foreign.C.Types (", T.unwords typeList, T.pack ")\n"]

tlist :: [T.Text]
tlist = map T.pack ["CInt","CInt", "CULLong", "CSChar", "CInt"]

-- | Example:
--
-- >>> ioifyUnit (T.pack "Bool")
-- "IO (Bool)"
ioifyUnit :: T.Text -> T.Text
ioifyUnit = textAp "IO"

-- | Example:
--
-- >>> fixUnit (T.pack "String")
-- "Fix (String)"
--
fixUnit :: T.Text -> T.Text
fixUnit = textAp "Fix"

-- | Example:
--
-- >>> ioifyTypeList . map T.pack ["Bool", "String", "Int"]
-- ["Bool", "String", "IO (Int)"]
--
ioifyTypeList :: [T.Text] -> [T.Text]
ioifyTypeList = apToLast ioifyUnit

-- | Example:
--
-- >>> fixInit . map T.pack $ ["a", "b", "c"]
-- "Fix (a) -> Fix (b)"
--
fixInit :: [T.Text] -> T.Text
fixInit = addArrows . map fixUnit . init

-- | Example:
--
-- >>> wrapperLast . map T.pack $ ["a", "b", "c"]
-- "Wrapper (FunPtr (a -> b -> c) (c))"
--
wrapperLast :: [T.Text] -> T.Text
wrapperLast = textAp "Wrapper" . liftM2 unwords2 (textAp "FunPtr" . addArrows) (parens . last)

-- fixTypeList =

-- | Example:
--
-- >>> fixTypeList (T.pack "a -> b -> c")
-- "Fix (a) -> Fix (b) -> Wrapper (FunPtr (a -> b -> c)) c"
--
fixTypeList :: [T.Text] -> T.Text
fixTypeList = liftM2 (appendAfter (T.pack " -> ")) fixInit wrapperLast

-- | Example:
--
-- >>> functionImport (T.pack "math.h") (T.pack "sin") (T.pack "CDouble -> CDouble")
-- "foreign import ccall \"math.h sin\" c_sin :: CDouble -> CDouble \n"
--
functionImport :: T.Text -> T.Text -> T.Text -> T.Text
functionImport header name ftype = T.unwords [T.pack "foreign import ccall",
                                              wrapText '\"' . T.concat $ [header, T.singleton ' ', name],
                                              T.append (T.pack "c_") name,
                                              T.pack "::",
                                              ftype,
                                              T.pack "\n"]


-- boolsToCUInt = foldl1 xor . zipWith (\p b -> if b then bit p else zeroBits) [0..]

-- c_func :: a -> b -> c
-- c_compilable_func ::

-- fixedToC :: Default a => Fix a -> a
-- fixedToC (Fixed x) = x
-- fixedToC Unfixed = def

-- boolsToCUInt :: [Bool] -> CUInt

-- func x y = if isFixed2 (x, y)
--               then Wrap undefined (c_func x y)
--               else Wrap (c_compilable_func fixity (f2C x) (f2C y)) undefined
--   where
--     f2C    = fixedToC
--     fixity = boolsToCUInt [isFixed x, isFixed y]

-- func x y z = if isFixed3 (x,y,z)
--                 then Wrap undefined (c_func x y)
--                 else Wrap (c_compilable_func fixity (f2C x) (f2C y) (f2C z)) undefined
--   where
--     fixity = boolsToCUInt [isFixed x, isFixed y, isFixed z]

importBits :: T.Text
importBits = T.pack "import Data.Bits (bit, xor, zeroBits)\n"

auxBoolToText :: Int -> T.Text
auxBoolToText pos = parens . T.concat $ [T.pack "if x", showInt pos, T.pack " then bit ", showInt (pos - 1), T.pack " else zeroBits"]

mkTup :: Int -> T.Text
mkTup n = parens . T.intercalate (T.singleton ',') . map (T.cons 'x' . showInt) $ [1..n]


-- NEED TO GENERATE TYPE: (Bool, Bool, .., Bool) -> CUInt
{-# WARNING mkBoolToCUInt "This function needs a specific type declaration, don't forget to implement!" #-}
mkBoolToCUInt :: Int -> T.Text
mkBoolToCUInt n = T.concat [T.pack "boolToCUInt", showInt n, T.singleton ' ', mkTup n, T.pack " = ", T.intercalate (T.pack " `xor` ") . map auxBoolToText $ [1..n]]

mkBoolToCUIntName :: Int -> T.Text
mkBoolToCUIntName n = T.append (T.pack "boolToCUInt") (showInt n)

andIsFixed :: Int -> T.Text
andIsFixed = T.intercalate (T.pack " && ") . map (T.append (T.pack "isFixed x") . showInt) . enumFromTo 1

mkIsFixed :: Int -> T.Text
mkIsFixed n = T.concat [T.pack "isFixed", T.intercalate (T.pack " x") (showInt n : map showInt [1..n]), T.pack " = ", andIsFixed n, T.singleton '\n']

mkIsFixedName :: Int -> T.Text
mkIsFixedName n = T.append (T.pack "isFixed") (showInt n)

-- isFixed2 x0 x1 = isFixed x0 && isFixed x1
-- isFixed3 x0 x1 x2 = isFixed x0 && isFixed x1 && isFixed x2
-- isFixed4 x0 x1 x2 x3 = isFixed x0 && isFixed x1 && isFixed x2 && isFixed x3
