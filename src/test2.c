#include <stdio.h>
#include "test2.h"

int add(int a, int b){return a + b;}

int (*functionPtr)(int,int) = &add;

int main(){
 unsigned char *p = (unsigned char *)&functionPtr;
    size_t i;

    for (i = 0; i < sizeof functionPtr; i++)
    {
        printf("%02x ", p[i]);
    }
    putchar('\n');
  getchar();
  return 0;
}
