module Main where

import Criterion.Main (defaultMain)
import Bench.HSGen.Recompiler.Class (benchHSGenRecompilerClass)
import Bench.Parse.Templates (benchParseTemplates)

main :: IO ()
main = defaultMain . concat $ [ benchHSGenRecompilerClass
                              , benchParseTemplates
                              ]
