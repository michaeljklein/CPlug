#include <stdio.h>

#define READ_UNIT(T) void T##_reader(T *buf, FILE *ptr){       \
                       buf[0] = (T) getc(ptr);                 \
                       for (int i = 1; i < sizeof(T); i++) {   \
                         buf[i-1] <<= 2;                       \
                         buf[i] ^= (T) getc(ptr);              \
                       }                                       \
                     }

#define WRITE_UNIT(T) void T##_writer(T *buf, FILE *ptr){                   \
                        T unit = *buf;                                      \
                        for (int i = 0; i < sizeof(T); i += 8) {                \
                          putc(unit & ((1 << (8 * sizeof(T))) - 1 ), ptr);  \
                          unit >>= 8;                                       \
                        }                                                   \
                      }


typedef signed char            signed_char;
typedef unsigned char          unsigned_char;
typedef short int              short_int;
typedef signed short           signed_short;
typedef signed short int       signed_short_int;
typedef unsigned short         unsigned_short;
typedef unsigned short int     unsigned_short_int;
typedef signed int             signed_int;
typedef unsigned int           unsigned_int;
typedef long int               long_int;
typedef signed long            signed_long;
typedef signed long int        signed_long_int;
typedef unsigned long          unsigned_long;
typedef unsigned long int      unsigned_long_int;
typedef long long              long_long;
typedef long long int          long_long_int;
typedef signed long long       signed_long_long;
typedef signed long long int   signed_long_long_int;
typedef unsigned long long     unsigned_long_long;
typedef unsigned long long int unsigned_long_long_int;
typedef long double            long_double;


READ_UNIT(char)
WRITE_UNIT(char)

READ_UNIT(signed_char)
WRITE_UNIT(signed_char)

READ_UNIT(unsigned_char)
WRITE_UNIT(unsigned_char)

READ_UNIT(short)
WRITE_UNIT(short)

READ_UNIT(short_int)
WRITE_UNIT(short_int)

READ_UNIT(signed_short)
WRITE_UNIT(signed_short)

READ_UNIT(signed_short_int)
WRITE_UNIT(signed_short_int)

READ_UNIT(unsigned_short)
WRITE_UNIT(unsigned_short)

READ_UNIT(unsigned_short_int)
WRITE_UNIT(unsigned_short_int)

READ_UNIT(int)
WRITE_UNIT(int)

READ_UNIT(signed)
WRITE_UNIT(signed)

READ_UNIT(signed_int)
WRITE_UNIT(signed_int)

READ_UNIT(unsigned)
WRITE_UNIT(unsigned)

READ_UNIT(unsigned_int)
WRITE_UNIT(unsigned_int)

READ_UNIT(long)
WRITE_UNIT(long)

READ_UNIT(long_int)
WRITE_UNIT(long_int)

READ_UNIT(signed_long)
WRITE_UNIT(signed_long)

READ_UNIT(signed_long_int)
WRITE_UNIT(signed_long_int)

READ_UNIT(unsigned_long)
WRITE_UNIT(unsigned_long)

READ_UNIT(unsigned_long_int)
WRITE_UNIT(unsigned_long_int)

READ_UNIT(long_long)
WRITE_UNIT(long_long)

READ_UNIT(long_long_int)
WRITE_UNIT(long_long_int)

READ_UNIT(signed_long_long)
WRITE_UNIT(signed_long_long)

READ_UNIT(signed_long_long_int)
WRITE_UNIT(signed_long_long_int)

READ_UNIT(unsigned_long_long)
WRITE_UNIT(unsigned_long_long)

READ_UNIT(unsigned_long_long_int)
WRITE_UNIT(unsigned_long_long_int)

/* READ_UNIT(float) */
/* WRITE_UNIT(float) */

/* READ_UNIT(double) */
/* WRITE_UNIT(double) */

/* READ_UNIT(long_double) */
/* WRITE_UNIT(long_double) */



int main(){
  FILE * fd = fopen("t.txt", "w");
  int outint;
  long outlongint;
  int_reader(&outint, fd);
  long_reader(&outlongint, fd);

  fclose(fd);
  return 0;
}


