#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <unistd.h>

uint64_t fibInit[4] = {1,1,1,0};

void sq(uint64_t * a){
  uint64_t x = a[0]^2 + a[1] * a[2];
  uint64_t y = a[1] *  (a[0] + a[3]);
  uint64_t z = a[2] *  (a[0] + a[3]);
  uint64_t w = a[1] *   a[2] + a[3] ^ 2;
  a[0] = x;
  a[1] = y;
  a[2] = z;
  a[3] = w;
}

void multTo(uint64_t * a, uint64_t * b){
  uint64_t x = a[0] * b[0] + a[2] * b[1];
  uint64_t y = a[1] * b[0] + a[3] * b[1];
  uint64_t z = a[0] * b[2] + a[2] * b[3];
  uint64_t w = a[1] * b[2] + a[3] * b[3];
  b[0] = x;
  b[1] = y;
  b[2] = z;
  b[3] = w;
}

uint64_t fib(uint64_t x0){
  uint64_t x = x0;
  uint64_t looper[4] = {1,1,1,0};
  uint64_t out[4]    = {1,0,0,1};
  while (x) {
    if (x & 1) {
      multTo(looper,out);
    }
    sq(looper);
    x = x >> 1;
  }
  return out[1];
}

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

int readUInt64(int fdescriptor, uint64_t *out){
  *out = 0;
  char buf;
  ssize_t err = 1;
  err = read(fdescriptor, &buf, 1);
  if (err > 0) {
    while ('0' <= buf && buf <= '9' && err > 0) {
      *out = (*out) * 10 + (buf - '0');
      err = read(fdescriptor, &buf, 1);
    }
    if (buf == '\0' && err > 0) {
      return 1;
    }
  }
  return 0;
}

int writeUInt64(int fdescriptor, uint64_t in){
  uint64_t out = in;
  char buf;
  ssize_t err = 1;
  while (out && (err > 0)) {
    buf = (char) out % 10 + '0';
    err = write(fdescriptor, &buf, 1);
    out /= 10;
  }
  if (err > 0) {
    buf = '\0';
    err = write(fdescriptor, &buf, 1);
  }
  return (err > 0);
}

int main(int argc, char *argv[]){
  if (argc < 3) {
    return 1;
  }

  int pipe_in_num  = parseInt(argv[1]);
  int pipe_out_num = parseInt(argv[2]);

  uint64_t input;
  int readStat  = readUInt64(pipe_in_num, &input);
  int writeStat = 0;
  if (readStat) {
    writeStat = writeUInt64(pipe_out_num, fib(input));
  } else {
    printf("Error, reading from pipe failed.\n");
  }

  if (writeStat == 0) {
    printf("Error, writing to pipe failed.\n");
  }

  /* uint64_t i = 0; */
  /* uint64_t rr = 0; */
  /* while (i < 10000000){ */
  /*   rr += fib(i); */
  /*   i++; */
  /* } */
  /* printf("%" PRIu64 "\n", rr); */
  return 0;
}

