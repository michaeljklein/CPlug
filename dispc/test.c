#include "test.h"
#include <stddef.h>
#include <stdio.h>

int cadd(int a, int b){return a + b;}

int cadd3(int a, int b, int c){return a + b + c;}

int (*functionPtr)(int,int,int) = &cadd3;

struct uchrWLen {unsigned char * ar; size_t len;};

typedef struct uchrWLen uchrAr;

unsigned char * fromFPtr(int (*fptr)(int,int,int)){
  size_t i;
  unsigned char *p = (unsigned char *)&fptr;
  unsigned char out[sizeof fptr];
  for (i = 0; i < sizeof fptr; i++){
    out[i] = p[i];
  }
  /* uchrAr outAr; */
  /* outAr.ar = out; */
  /* outAr.len = sizeof fptr; */
  return out;
}

int (*toFPtr(unsigned char * in))(int,int,int){
  unsigned char *out = in;
  int (*fout)(int,int,int) = (int(*)(int,int,int)) out;
  return fout;
}

int main(){
 unsigned char * tt = fromFPtr(cadd3);
 int (*ttt)(int,int,int) = toFPtr(tt);
 printf("test, 1 + 2 + 3 = %d\n\n", (*ttt)(1,2,3));
 /* unsigned char *p = (unsigned char *)&functionPtr; */
 /*    size_t i; */

 /*    for (i = 0; i < sizeof functionPtr; i++) */
 /*    { */
 /*        printf("%02x ", p[i]); */
 /*    } */
 /*    putchar('\n'); */
 /*  getchar(); */
  return 0;
}
