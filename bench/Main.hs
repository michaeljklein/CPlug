module Main where

import Bench.Data.Undefined (benchDataUndefined)
import Bench.HSGen.Recompiler.Class (benchHSGenRecompilerClass)

main = do
  benchDataUndefined
  benchHSGenRecompilerClass
  print "done."
