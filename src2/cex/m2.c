#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
 
int main(void) {
	unsigned long long x = 4660046610375530309ULL;
	char * y = malloc(8);
  y = memcpy(y, &x, 8);
	printf("%u\n", y[0]);
	printf("%u\n", y[1]);
	printf("%u\n", y[2]);
	printf("%u\n", y[3]);
	printf("%u\n", y[4]);
	printf("%u\n", y[5]);
	printf("%u\n", y[6]);
	printf("%u\n", y[7]);
	return 0;
}
