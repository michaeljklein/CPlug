# CPlug

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

