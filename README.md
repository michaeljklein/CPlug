# CPlug

Note: I set up a small pre-commit script that runs `cloc . | tee stats.txt` to print/show how many lines of code per language, etc. If you are committing, either install CLOC (Count Lines of Code), which can be installed with `brew install cloc`, or delete the script in `.git/hooks`.

Plan of attack:
 1)   User submits .c/.h files
 2)   Individual functions are found (Language.C.Parser)
    2a) Dependencies of each function found
    2b) Each function is isolated with its dependencies
 3)   The input/return types of each function are found
 4)   A piped caller is generated (in c) for each function
    4a) The piped caller could support null/Nothing arguments
 5)   A piped runner (c) template (hs) is generated
 6)   Haskell FFI for the original function/piped caller is generated
 7)   Haskell function to allow recompilation is generated from (3), (5), (6)
 8)   The compilable (hs) function supports:
    8a) Resolution to value if all Fixed (else undefined)
    8b) Resolution to recompiled function
 9)   If function is to be recompiled, generate .c (piped runner) code from template
10)   Call gcc/clang on piped runner.c file
11)   If errors, return FFI (unpiped) reference, else instantiate piped runner with reference to output of compilation
12)   When piped caller is called, arguments are packed into c-string and written to pipe
13)   Piped runner is called on pipe (as argv)
14)   Piped caller receives result of piped runner
15)   Piped caller unpacks c-string from pipe and returns to Haskell
16)   Result of computation is received by Haskell


Goals:
- Generate a Haskell Module from a .c file
- Automatically generate (HaskellCType a -> CTypeString) functions
- Have imported functions be recompilable
- Be able to apply arguments to the C functions in any order and recompile
- Be able to strip "pure" C function from Compilable version
- Be able to mark C functions as safe/unsafe within the .c file
- Be able to extract single functions from a .c file
- Be able to pull used functions from headers and drop rest
- Have a natural interface to use the Compilable C functions within Haskell
- Be type safe, if compilation fails, return original function (which will always exist since a version will have to compile for the module to compile)
- Utilize carray
- Be fast

Antigoals:
- Optimize C or Haskell code
- Parse anything other than declarations (ok, maybe "mentions" of things in body, but only if I don't roll my own parser)
- Generate C code with different syntax on the fly
- Modify the types of the Haskell module functions (except for application/extraction)
- Get fancy with compilation options
- Use TemplateHaskell
- Worry about structs

Possible Goals:
- Allow the passing/accepting of (limited) Haskell types by the C functions
- Detect most common arguments (based on last N in stream) and automatically recompile

