#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

int main(){
  void *handle;
  int (*main2)();
  char *error;

  handle = dlopen ("/Users/michaelklein/Coding/dispc/src/cex2/outf", RTLD_NOW);
  if (!handle) {
      fputs (dlerror(), stderr);
      printf("\n"); 
      exit(1);
  }

  main2 = dlsym(handle, "main");
  if ((error = dlerror()) != NULL)  {
      fputs(error, stderr);
      printf("\n");
      exit(1);
  }

  main2();
  dlclose(handle);
}
