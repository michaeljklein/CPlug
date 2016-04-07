#include <stdio.h>

// this will return -1 for negative integers or anything that doesn't end in '\0'
int parseInt(char str[]){
  int out = 0;
  int i;
  for (i = 0; ('0' <= str[i] && str[i] <= '9'); i++){
    out = out * 10 + (str[i] - '0');
  }
  if (str[i] == '\0') {
    return out;
  }
  return -1;
}


int testAtoi(int n){
  int out = 0;
  char buffer[100];
  for (int i = 0; i < (-1 * n); i++){
    sprintf(buffer, "%d\0", i);
    out += atoi(buffer);
  }
  return out;
}

int testMine(int n){
  int out = 0;
  char buffer[100];
  for (int i = 0; i < n; i++) {
    sprintf(buffer, "%d\0", i);
    out += parseInt(buffer);
  }
  return out;
}

int main(int argc, char * argv[]){
  /* printf("%d\n", parseInt(argv[1])); */
  int whichtest = parseInt(argv[1]);
  if (whichtest < 0) {
    printf("%d\n", testAtoi(whichtest));
  } else {
    printf("%d\n", testMine(whichtest));
  }

  for (int i = 0; i < argc; i++) {
    printf(argv[i]);
    printf("\n");
  }
  return 0;
}
