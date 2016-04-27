
#include <dlfcn.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>


#define DEBUG 1

#ifdef DEBUG
  #define RAISE_ERROR(error) fputs((error), stderr);exit(1);
#else
  #define RAISE_ERROR(error)
#endif



int sum_func(int a, int b){
  return a + b;
}

const char *filename_string = "path/filename";
void *filename_handle = NULL;

void run_compiler_etc(){}

// If no `null_vars`, the file loading fails, or the symbol loading fails, the original function pointer is returned
int (*get_sum_func(int null_vars, int a, int b))(int, int){
  if (null_vars) {
    run_compiler_etc;
    filename_handle = dlopen(filename_string, RTLD_LAZY); // RTLD_NOW is safer, but slower
    if (filename_handle) {
      int (*sum_func_ptr)(int,int) = dlsym(filename_handle, "sum_func");
      char *error = dlerror();
      if (error != NULL) {
        RAISE_ERROR(error);
      } else {
        return sum_func_ptr;
      }
    }
  }
  return &sum_func;
}

// main functions included, series of wrappers/pointers (i.e. functions that 
