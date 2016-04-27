module Data.Undefined (isUndefined) where

-- Note: This is it's own module for a couple reasons:
-- 1. I can't guarantee its safety or that it will last
--    ("System.Mem.StableName" is experimental)
-- 2. It really needs it's own seperate series of tests
-- 3. Although, how it's used in this module, `undefined` will never be
--    returned by a function and so one could argue that it's not a complete
--    abuse of the type system, it's still an abuse of the type system.

import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (eqStableName, makeStableName)

isUndefined :: a -> Bool
isUndefined = (.) =<< eqStableName . ($ undefined) $ unsafePerformIO . makeStableName
