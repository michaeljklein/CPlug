{-# DataKinds #-}
import System.Environment (getArgs)
data Nat = Zero | Succ Nat

toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n-1))

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

-- tester = fromNat . toNat

data N a = Z | T (Z,N a)

fromNat2 :: N a -> Int
fromNat2 Z = 0
fromNat2 (T (_,z)) = 1 + fromNat2 z

toNat2 :: Int -> N a
toNat2 0 = Z
toNat2 n = T (Z,toNat2 (n-1))

tester = fromNat2.toNat2

main = do
  args <- getArgs
  let arg = head args
  let n = read arg :: Int
  print $ tester n
