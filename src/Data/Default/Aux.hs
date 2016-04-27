{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Default.Aux (Default(..)) where

import Data.Default
import Foreign.Ptr (Ptr, FunPtr, IntPtr, WordPtr, nullPtr, nullFunPtr,
                    ptrToIntPtr, ptrToWordPtr)
import Data.Bits (Bits, zeroBits)


instance Default (Ptr a) where
  def = nullPtr

instance Default (FunPtr a) where
  def = nullFunPtr

instance Default IntPtr where
  def = ptrToIntPtr nullPtr

instance Default WordPtr where
  def = ptrToWordPtr nullPtr

instance {-# OVERLAPPABLE #-} Bits a => Default a where
  def = zeroBits
