module CGen where

--  CGen
--    makeTemplate :: CFunctionSource -> CFunctionTemplate


--  CGen.Pipe
--

--  CGen.Pipe.Read
--    generate C parser for read()

--  CGen.Pipe.Write
--    generate C sprintf for write()

--  CGen.Pipe.Caller
--    convert CFunctionSource to pipe caller CFunctionSource
--    support null/nothing types
--
-- pipe_caller_funname(char which_are_blank, args..){
--   if (which_are_blank & 1){ pipe_arg_1() }
--   if (which_are_blank & 2){ pipe_arg_1() }
--   ..
-- }

--  CGen.Pipe.Runner
--    convert CFunctionSource to pipe runner PipeRunnerTemplate

{- Read/Write implementation notes:
  Pointer input/output is not supported. Simply ignore (except for dependency) anything that has a pointer as input/output or in a struct in input/output
  Unsigned types can be split/unsplit from chars easily
  Signed types can be gotten by accessing bits directly
  (Both methods need number of bits in implementation (C FFI to sizet of all types?))
  Arrays sent as header with length then stream of above types
  Structs sent in alphabetical order
-}
