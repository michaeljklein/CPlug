# CPlug

Note: I set up a small pre-commit script that runs `cloc` and `tree` to show the number of lines of code per language and the directory tree. If you are committing, either install CLOC (Count Lines of Code) and tree, which can be installed with `brew install cloc`, `brew install tree`, or delete the script in `.git/hooks`.

Plan of attack:
 1.   User submits .c/.h files
 2.   Individual functions are found (Language.C.Parser)
    2.1. Dependencies of each function found
    2.2. Each function is isolated with its dependencies
 3.   The input/return types of each function are found
 4.   A piped caller is generated (in c) for each function
    4a) The piped caller could support null/Nothing arguments
 5.   A piped runner (c) template (hs) is generated
 6.   Haskell FFI for the original function/piped caller is generated
 7.   Haskell function to allow recompilation is generated from (3), (5), (6)
 8.   The compilable (hs) function supports:
    8.1. Resolution to value if all Fixed (else undefined)
    8.2. Resolution to recompiled function
 9.   If function is to be recompiled, generate .c (piped runner) code from template
10.   Call gcc/clang on piped runner.c file
11.   If errors, return FFI (unpiped) reference, else instantiate piped runner with reference to output of compilation
12.   When piped caller is called, arguments are packed into c-string and written to pipe
13.   Piped runner is called on pipe (as argv)
14.   Piped caller receives result of piped runner
15.   Piped caller unpacks c-string from pipe and returns to Haskell
16.   Result of computation is received by Haskell

Acheived:
- Be able to apply arguments to the C functions in any order and recompile
- Be able to strip "pure" C function from Compilable version


Goals:
- Generate a Haskell Module from a .c file
- Automatically generate (HaskellCType a -> CTypeString) functions (should probably double-check, but `Data.Text.Lazy.Builder.Int` could probably cover almost everything..)
- Have imported functions be recompilable
- Be able to mark C functions as safe/unsafe within the .c file
- Be able to extract single functions from a .c file
- Have a natural interface to use the Compilable C functions within Haskell (almost there..)
- Be type safe, if compilation fails, return original function (which will always exist since a version will have to compile for the module to compile)
- Utilize carray
- Be fast (fast calling, relative to native C/Haskell, will not happen. Pipes are not too shabby though, iirc they clocked ~10s for ~30 MB on "average" machines.)

Antigoals:
- Optimize C or Haskell code (except what gcc/clang does)
- Parse anything other than declarations (ok, maybe "mentions" of things in body, but only if I don't roll my own parser)
- Generate C code with different syntax on the fly
- Modify the types of the Haskell module functions (except for application/extraction)
- Get fancy with compilation options (actually, considering a `CompilationOptions` datatype that would cover this, but still push any non-default configuration to the user)
- Use TemplateHaskell
- Worry about structs (not going to worry, but Language.C should be able to handle them without too much effort)
- Detect most common arguments (based on last N in stream) and automatically recompile (since the project has switched slightly to leave out the "call lots of times in a row quickly" use-case, this is now an antigoal).

Possible Goals:
- Allow the passing/accepting of (limited) Haskell types by the C functions (would it be as easy as ... no, probably not, no matter how you finish that).
- Be able to pull used functions from headers and drop rest (might be out of scope of the project and really not necessary as pointer functions are dropped anyway, such as `main()`)
- By the time this is finished, there could be enough interface to generate SWIG bindings without too much trouble so, when the time comes, I'll look into that.
