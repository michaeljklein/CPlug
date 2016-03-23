#include <stdio.h>

int add(int a, int b){
  return a+b;
}

int tri(int a){
  int r = 0;
  for (int i = 1; i <= a; i++) {
    r = add(r, i);
  }
  return r;
}

main(){
  printf("Five triangular numbers: \n");
  for (int i = 1; i < 6; i++){
    printf("%d\n", tri(i));
  }
  printf("Test: 2 + 2 = %d\n", add(2,2));
  
  return 0;
}

