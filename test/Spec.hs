
-- include files from: https://github.com/gcc-mirror/gcc/tree/master/gcc/testsuite/gcc.dg/atomic
-- This is a group of "hard" examples, used for testing gcc.
-- It should be relatively easy to make a function that selects non-void
-- functions and runs a test to check that compiled/non-compiled functions
-- are functionally equivalent (though should probably do something like run
-- main() to ensure background vars/etc. are initialized(?)

main :: IO ()
main = do
  print "done."
